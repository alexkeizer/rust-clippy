use std::collections::BTreeSet;
use std::sync::Mutex;
use clippy_utils::diagnostics::span_lint_and_sugg;
use clippy_utils::source::snippet_with_applicability;
use clippy_utils::ty::is_type_lang_item;
use rustc_errors::Applicability;
use rustc_hir::{Expr, LangItem};
use rustc_lint::LateContext;
use rustc_span::{Span, BytePos};

use super::BYTES_NTH;

const LINTED_AT: Mutex<BTreeSet<BytePos>> = Mutex::new(BTreeSet::new());

fn check_recv_type<'tcx>(cx: &LateContext<'tcx>, recv: &'tcx Expr<'tcx>) -> Option<&'static str> {
    let ty = cx.typeck_results().expr_ty(recv).peel_refs();
    if ty.is_str() {
        Some("str")
    } else if is_type_lang_item(cx, ty, LangItem::String) {
        Some("String")
    } else {
        None
    }
}

pub(super) fn check<'tcx>(cx: &LateContext<'tcx>, expr: &Expr<'_>, recv: &'tcx Expr<'tcx>, n_arg: &'tcx Expr<'tcx>, span: Span) {
    let caller_type = match check_recv_type(cx, recv) {
        Some(ty) => ty,
        None => return,
    };
    let already_linted =
        LINTED_AT.lock()
            .map(|s|
                s.contains(&span.data().lo)
            )
            .unwrap_or(false);
    if already_linted {
        return;
    }

    let mut applicability = Applicability::MachineApplicable;
    span_lint_and_sugg(
        cx,
        BYTES_NTH,
        expr.span,
        &format!("called `.bytes().nth()` on a `{caller_type}`"),
        "try",
        format!(
            "{}.as_bytes().get({})",
            snippet_with_applicability(cx, recv.span, "..", &mut applicability),
            snippet_with_applicability(cx, n_arg.span, "..", &mut applicability)
        ),
        applicability,
    );
}

pub(super) fn check_unwrap<'tcx>(
    cx: &LateContext<'tcx>,
    expr: &Expr<'_>,
    recv: &'tcx Expr<'tcx>,
    n_arg: &'tcx Expr<'tcx>,
) {
    let caller_type = match check_recv_type(cx, recv) {
        Some(ty) => ty,
        None => return,
    };
    let mut applicability = Applicability::MachineApplicable;
    span_lint_and_sugg(
        cx,
        BYTES_NTH,
        expr.span,
        &format!("called `.bytes().nth().unwrap()` on a `{caller_type}`"),
        "try",
        format!(
            "{}.as_bytes()[{}]",
            snippet_with_applicability(cx, recv.span, "..", &mut applicability),
            snippet_with_applicability(cx, n_arg.span, "..", &mut applicability)
        ),
        applicability,
    );
}
