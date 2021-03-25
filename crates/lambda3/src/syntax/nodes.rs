use super::{
    ast::{self, support, AstChildren, AstNode},
    SyntaxKind::{self, *},
    SyntaxNode, SyntaxToken,
};
use crate::T;
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParenTy {
    pub(crate) syntax: SyntaxNode,
}
impl ParenTy {
    pub fn l_paren_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T!['('])
    }
    pub fn ty(&self) -> Option<Type> {
        support::child(&self.syntax)
    }
    pub fn r_paren_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![')'])
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IntTy {
    pub(crate) syntax: SyntaxNode,
}
impl IntTy {
    pub fn Int_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![Int])
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BoolTy {
    pub(crate) syntax: SyntaxNode,
}
impl BoolTy {
    pub fn Bool_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![Bool])
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FuncTy {
    pub(crate) syntax: SyntaxNode,
}
impl FuncTy {
    pub fn arg(&self) -> Option<TyArg> {
        support::child(&self.syntax)
    }
    pub fn arrow_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![->])
    }
    pub fn result(&self) -> Option<TyRes> {
        support::child(&self.syntax)
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyArg {
    pub(crate) syntax: SyntaxNode,
}
impl TyArg {
    pub fn ty(&self) -> Option<Type> {
        support::child(&self.syntax)
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyRes {
    pub(crate) syntax: SyntaxNode,
}
impl TyRes {
    pub fn ty(&self) -> Option<Type> {
        support::child(&self.syntax)
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WildcardP {
    pub(crate) syntax: SyntaxNode,
}
impl WildcardP {
    pub fn underscore_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![_])
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParenP {
    pub(crate) syntax: SyntaxNode,
}
impl ParenP {
    pub fn l_paren_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T!['('])
    }
    pub fn pattern(&self) -> Option<Pattern> {
        support::child(&self.syntax)
    }
    pub fn r_paren_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![')'])
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VarP {
    pub(crate) syntax: SyntaxNode,
}
impl VarP {
    pub fn ident_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![ident])
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AnnotationP {
    pub(crate) syntax: SyntaxNode,
}
impl AnnotationP {
    pub fn pattern(&self) -> Option<Pattern> {
        support::child(&self.syntax)
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![:])
    }
    pub fn ty(&self) -> Option<Type> {
        support::child(&self.syntax)
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LiteralE {
    pub(crate) syntax: SyntaxNode,
}
impl LiteralE {
    pub fn number_lit_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![number_lit])
    }
    pub fn false_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![false])
    }
    pub fn true_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![true])
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VarE {
    pub(crate) syntax: SyntaxNode,
}
impl VarE {
    pub fn ident_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![ident])
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LambdaE {
    pub(crate) syntax: SyntaxNode,
}
impl LambdaE {
    pub fn backslash_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![backslash])
    }
    pub fn binder(&self) -> Option<AnnotationP> {
        support::child(&self.syntax)
    }
    pub fn dot_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![.])
    }
    pub fn body(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ApplicationE {
    pub(crate) syntax: SyntaxNode,
}
impl ApplicationE {
    pub fn func(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
    pub fn arg(&self) -> Option<ExprArg> {
        support::child(&self.syntax)
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParenE {
    pub(crate) syntax: SyntaxNode,
}
impl ParenE {
    pub fn l_paren_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T!['('])
    }
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
    pub fn r_paren_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![')'])
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LetE {
    pub(crate) syntax: SyntaxNode,
}
impl LetE {
    pub fn let_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![let])
    }
    pub fn pattern(&self) -> Option<Pattern> {
        support::child(&self.syntax)
    }
    pub fn eq_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![=])
    }
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
    pub fn in_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![in])
    }
    pub fn body(&self) -> Option<ExprLetBody> {
        support::child(&self.syntax)
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExprArg {
    pub(crate) syntax: SyntaxNode,
}
impl ExprArg {
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExprLetBody {
    pub(crate) syntax: SyntaxNode,
}
impl ExprLetBody {
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    ParenTy(ParenTy),
    IntTy(IntTy),
    BoolTy(BoolTy),
    FuncTy(FuncTy),
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pattern {
    WildcardP(WildcardP),
    ParenP(ParenP),
    VarP(VarP),
    AnnotationP(AnnotationP),
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    LiteralE(LiteralE),
    VarE(VarE),
    LambdaE(LambdaE),
    ApplicationE(ApplicationE),
    ParenE(ParenE),
    LetE(LetE),
}
impl AstNode for ParenTy {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == PAREN_TY
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
impl AstNode for IntTy {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == INT_TY
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
impl AstNode for BoolTy {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == BOOL_TY
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
impl AstNode for FuncTy {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == FUNC_TY
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
impl AstNode for TyArg {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == TY_ARG
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
impl AstNode for TyRes {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == TY_RES
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
impl AstNode for WildcardP {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == WILDCARD_P
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
impl AstNode for ParenP {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == PAREN_P
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
impl AstNode for VarP {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == VAR_P
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
impl AstNode for AnnotationP {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == ANNOTATION_P
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
impl AstNode for LiteralE {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == LITERAL_E
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
impl AstNode for VarE {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == VAR_E
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
impl AstNode for LambdaE {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == LAMBDA_E
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
impl AstNode for ApplicationE {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == APPLICATION_E
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
impl AstNode for ParenE {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == PAREN_E
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
impl AstNode for LetE {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == LET_E
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
impl AstNode for ExprArg {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == EXPR_ARG
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
impl AstNode for ExprLetBody {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == EXPR_LET_BODY
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
impl From<ParenTy> for Type {
    fn from(node: ParenTy) -> Type {
        Type::ParenTy(node)
    }
}
impl From<IntTy> for Type {
    fn from(node: IntTy) -> Type {
        Type::IntTy(node)
    }
}
impl From<BoolTy> for Type {
    fn from(node: BoolTy) -> Type {
        Type::BoolTy(node)
    }
}
impl From<FuncTy> for Type {
    fn from(node: FuncTy) -> Type {
        Type::FuncTy(node)
    }
}
impl AstNode for Type {
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            PAREN_TY | INT_TY | BOOL_TY | FUNC_TY => true,
            _ => false,
        }
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        let res = match syntax.kind() {
            PAREN_TY => Type::ParenTy(ParenTy { syntax }),
            INT_TY => Type::IntTy(IntTy { syntax }),
            BOOL_TY => Type::BoolTy(BoolTy { syntax }),
            FUNC_TY => Type::FuncTy(FuncTy { syntax }),
            _ => return None,
        };
        Some(res)
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Type::ParenTy(it) => &it.syntax,
            Type::IntTy(it) => &it.syntax,
            Type::BoolTy(it) => &it.syntax,
            Type::FuncTy(it) => &it.syntax,
        }
    }
}
impl From<WildcardP> for Pattern {
    fn from(node: WildcardP) -> Pattern {
        Pattern::WildcardP(node)
    }
}
impl From<ParenP> for Pattern {
    fn from(node: ParenP) -> Pattern {
        Pattern::ParenP(node)
    }
}
impl From<VarP> for Pattern {
    fn from(node: VarP) -> Pattern {
        Pattern::VarP(node)
    }
}
impl From<AnnotationP> for Pattern {
    fn from(node: AnnotationP) -> Pattern {
        Pattern::AnnotationP(node)
    }
}
impl AstNode for Pattern {
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            WILDCARD_P | PAREN_P | VAR_P | ANNOTATION_P => true,
            _ => false,
        }
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        let res = match syntax.kind() {
            WILDCARD_P => Pattern::WildcardP(WildcardP { syntax }),
            PAREN_P => Pattern::ParenP(ParenP { syntax }),
            VAR_P => Pattern::VarP(VarP { syntax }),
            ANNOTATION_P => Pattern::AnnotationP(AnnotationP { syntax }),
            _ => return None,
        };
        Some(res)
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Pattern::WildcardP(it) => &it.syntax,
            Pattern::ParenP(it) => &it.syntax,
            Pattern::VarP(it) => &it.syntax,
            Pattern::AnnotationP(it) => &it.syntax,
        }
    }
}
impl From<LiteralE> for Expr {
    fn from(node: LiteralE) -> Expr {
        Expr::LiteralE(node)
    }
}
impl From<VarE> for Expr {
    fn from(node: VarE) -> Expr {
        Expr::VarE(node)
    }
}
impl From<LambdaE> for Expr {
    fn from(node: LambdaE) -> Expr {
        Expr::LambdaE(node)
    }
}
impl From<ApplicationE> for Expr {
    fn from(node: ApplicationE) -> Expr {
        Expr::ApplicationE(node)
    }
}
impl From<ParenE> for Expr {
    fn from(node: ParenE) -> Expr {
        Expr::ParenE(node)
    }
}
impl From<LetE> for Expr {
    fn from(node: LetE) -> Expr {
        Expr::LetE(node)
    }
}
impl AstNode for Expr {
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            LITERAL_E | VAR_E | LAMBDA_E | APPLICATION_E | PAREN_E | LET_E => true,
            _ => false,
        }
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        let res = match syntax.kind() {
            LITERAL_E => Expr::LiteralE(LiteralE { syntax }),
            VAR_E => Expr::VarE(VarE { syntax }),
            LAMBDA_E => Expr::LambdaE(LambdaE { syntax }),
            APPLICATION_E => Expr::ApplicationE(ApplicationE { syntax }),
            PAREN_E => Expr::ParenE(ParenE { syntax }),
            LET_E => Expr::LetE(LetE { syntax }),
            _ => return None,
        };
        Some(res)
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Expr::LiteralE(it) => &it.syntax,
            Expr::VarE(it) => &it.syntax,
            Expr::LambdaE(it) => &it.syntax,
            Expr::ApplicationE(it) => &it.syntax,
            Expr::ParenE(it) => &it.syntax,
            Expr::LetE(it) => &it.syntax,
        }
    }
}
impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for ParenTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for IntTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for BoolTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for FuncTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for TyArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for TyRes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for WildcardP {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for ParenP {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for VarP {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for AnnotationP {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for LiteralE {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for VarE {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for LambdaE {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for ApplicationE {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for ParenE {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for LetE {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for ExprArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for ExprLetBody {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
