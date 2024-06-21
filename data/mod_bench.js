// Source: https://github.com/microsoft/TypeScript
// Commit: c3f185417ac874f872d9b4c00af40f56b6790fd0
// File: src/compiler/transformers/ts.ts

/*
The below license applies only to the contents of this file.

Apache License

Version 2.0, January 2004

http://www.apache.org/licenses/

TERMS AND CONDITIONS FOR USE, REPRODUCTION, AND DISTRIBUTION

1. Definitions.

"License" shall mean the terms and conditions for use, reproduction, and distribution as defined by Sections 1 through 9 of this document.

"Licensor" shall mean the copyright owner or entity authorized by the copyright owner that is granting the License.

"Legal Entity" shall mean the union of the acting entity and all other entities that control, are controlled by, or are under common control with that entity. For the purposes of this definition, "control" means (i) the power, direct or indirect, to cause the direction or management of such entity, whether by contract or otherwise, or (ii) ownership of fifty percent (50%) or more of the outstanding shares, or (iii) beneficial ownership of such entity.

"You" (or "Your") shall mean an individual or Legal Entity exercising permissions granted by this License.

"Source" form shall mean the preferred form for making modifications, including but not limited to software source code, documentation source, and configuration files.

"Object" form shall mean any form resulting from mechanical transformation or translation of a Source form, including but not limited to compiled object code, generated documentation, and conversions to other media types.

"Work" shall mean the work of authorship, whether in Source or Object form, made available under the License, as indicated by a copyright notice that is included in or attached to the work (an example is provided in the Appendix below).

"Derivative Works" shall mean any work, whether in Source or Object form, that is based on (or derived from) the Work and for which the editorial revisions, annotations, elaborations, or other modifications represent, as a whole, an original work of authorship. For the purposes of this License, Derivative Works shall not include works that remain separable from, or merely link (or bind by name) to the interfaces of, the Work and Derivative Works thereof.

"Contribution" shall mean any work of authorship, including the original version of the Work and any modifications or additions to that Work or Derivative Works thereof, that is intentionally submitted to Licensor for inclusion in the Work by the copyright owner or by an individual or Legal Entity authorized to submit on behalf of the copyright owner. For the purposes of this definition, "submitted" means any form of electronic, verbal, or written communication sent to the Licensor or its representatives, including but not limited to communication on electronic mailing lists, source code control systems, and issue tracking systems that are managed by, or on behalf of, the Licensor for the purpose of discussing and improving the Work, but excluding communication that is conspicuously marked or otherwise designated in writing by the copyright owner as "Not a Contribution."

"Contributor" shall mean Licensor and any individual or Legal Entity on behalf of whom a Contribution has been received by Licensor and subsequently incorporated within the Work.

2. Grant of Copyright License. Subject to the terms and conditions of this License, each Contributor hereby grants to You a perpetual, worldwide, non-exclusive, no-charge, royalty-free, irrevocable copyright license to reproduce, prepare Derivative Works of, publicly display, publicly perform, sublicense, and distribute the Work and such Derivative Works in Source or Object form.

3. Grant of Patent License. Subject to the terms and conditions of this License, each Contributor hereby grants to You a perpetual, worldwide, non-exclusive, no-charge, royalty-free, irrevocable (except as stated in this section) patent license to make, have made, use, offer to sell, sell, import, and otherwise transfer the Work, where such license applies only to those patent claims licensable by such Contributor that are necessarily infringed by their Contribution(s) alone or by combination of their Contribution(s) with the Work to which such Contribution(s) was submitted. If You institute patent litigation against any entity (including a cross-claim or counterclaim in a lawsuit) alleging that the Work or a Contribution incorporated within the Work constitutes direct or contributory patent infringement, then any patent licenses granted to You under this License for that Work shall terminate as of the date such litigation is filed.

4. Redistribution. You may reproduce and distribute copies of the Work or Derivative Works thereof in any medium, with or without modifications, and in Source or Object form, provided that You meet the following conditions:

You must give any other recipients of the Work or Derivative Works a copy of this License; and

You must cause any modified files to carry prominent notices stating that You changed the files; and

You must retain, in the Source form of any Derivative Works that You distribute, all copyright, patent, trademark, and attribution notices from the Source form of the Work, excluding those notices that do not pertain to any part of the Derivative Works; and

If the Work includes a "NOTICE" text file as part of its distribution, then any Derivative Works that You distribute must include a readable copy of the attribution notices contained within such NOTICE file, excluding those notices that do not pertain to any part of the Derivative Works, in at least one of the following places: within a NOTICE text file distributed as part of the Derivative Works; within the Source form or documentation, if provided along with the Derivative Works; or, within a display generated by the Derivative Works, if and wherever such third-party notices normally appear. The contents of the NOTICE file are for informational purposes only and do not modify the License. You may add Your own attribution notices within Derivative Works that You distribute, alongside or as an addendum to the NOTICE text from the Work, provided that such additional attribution notices cannot be construed as modifying the License. You may add Your own copyright statement to Your modifications and may provide additional or different license terms and conditions for use, reproduction, or distribution of Your modifications, or for any such Derivative Works as a whole, provided Your use, reproduction, and distribution of the Work otherwise complies with the conditions stated in this License.

5. Submission of Contributions. Unless You explicitly state otherwise, any Contribution intentionally submitted for inclusion in the Work by You to the Licensor shall be under the terms and conditions of this License, without any additional terms or conditions. Notwithstanding the above, nothing herein shall supersede or modify the terms of any separate license agreement you may have executed with Licensor regarding such Contributions.

6. Trademarks. This License does not grant permission to use the trade names, trademarks, service marks, or product names of the Licensor, except as required for reasonable and customary use in describing the origin of the Work and reproducing the content of the NOTICE file.

7. Disclaimer of Warranty. Unless required by applicable law or agreed to in writing, Licensor provides the Work (and each Contributor provides its Contributions) on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied, including, without limitation, any warranties or conditions of TITLE, NON-INFRINGEMENT, MERCHANTABILITY, or FITNESS FOR A PARTICULAR PURPOSE. You are solely responsible for determining the appropriateness of using or redistributing the Work and assume any risks associated with Your exercise of permissions under this License.

8. Limitation of Liability. In no event and under no legal theory, whether in tort (including negligence), contract, or otherwise, unless required by applicable law (such as deliberate and grossly negligent acts) or agreed to in writing, shall any Contributor be liable to You for damages, including any direct, indirect, special, incidental, or consequential damages of any character arising as a result of this License or out of the use or inability to use the Work (including but not limited to damages for loss of goodwill, work stoppage, computer failure or malfunction, or any and all other commercial damages or losses), even if such Contributor has been advised of the possibility of such damages.

9. Accepting Warranty or Additional Liability. While redistributing the Work or Derivative Works thereof, You may choose to offer, and charge a fee for, acceptance of support, warranty, indemnity, or other liability obligations and/or rights consistent with this License. However, in accepting such obligations, You may act only on Your own behalf and on Your sole responsibility, not on behalf of any other Contributor, and only if You agree to indemnify, defend, and hold each Contributor harmless for any liability incurred by, or claims asserted against, such Contributor by reason of your accepting any such warranty or additional liability.

END OF TERMS AND CONDITIONS
*/

export function transformTypeScript(context) {
  const {
    factory,
    getEmitHelperFactory: emitHelpers,
    startLexicalEnvironment,
    resumeLexicalEnvironment,
    endLexicalEnvironment,
    hoistVariableDeclaration,
  } = context;
  const resolver = context.getEmitResolver();
  const compilerOptions = context.getCompilerOptions();
  const languageVersion = getEmitScriptTarget(compilerOptions);
  const moduleKind = getEmitModuleKind(compilerOptions);
  const legacyDecorators = !!compilerOptions.experimentalDecorators;
  const typeSerializer = compilerOptions.emitDecoratorMetadata
    ? createRuntimeTypeSerializer(context)
    : undefined;
  const previousOnEmitNode = context.onEmitNode;
  const previousOnSubstituteNode = context.onSubstituteNode;
  context.onEmitNode = onEmitNode;
  context.onSubstituteNode = onSubstituteNode;
  context.enableSubstitution(SyntaxKind.PropertyAccessExpression);
  context.enableSubstitution(SyntaxKind.ElementAccessExpression);
  let currentSourceFile;
  let currentNamespace;
  let currentNamespaceContainerName;
  let currentLexicalScope;
  let currentScopeFirstDeclarationsOfName;
  let currentClassHasParameterProperties;
  let enabledSubstitutions;
  let applicableSubstitutions;
  return transformSourceFileOrBundle;
  function transformSourceFileOrBundle(node) {
    if (node.kind === SyntaxKind.Bundle) {
      return transformBundle(node);
    }
    return transformSourceFile(node);
  }
  function transformBundle(node) {
    return factory.createBundle(node.sourceFiles.map(transformSourceFile));
  }
  function transformSourceFile(node) {
    if (node.isDeclarationFile) {
      return node;
    }
    currentSourceFile = node;
    const visited = saveStateAndInvoke(node, visitSourceFile);
    addEmitHelpers(visited, context.readEmitHelpers());
    currentSourceFile = undefined;
    return visited;
  }
  function saveStateAndInvoke(node, f) {
    const savedCurrentScope = currentLexicalScope;
    const savedCurrentScopeFirstDeclarationsOfName =
      currentScopeFirstDeclarationsOfName;
    const savedCurrentClassHasParameterProperties =
      currentClassHasParameterProperties;
    onBeforeVisitNode(node);
    const visited = f(node);
    if (currentLexicalScope !== savedCurrentScope) {
      currentScopeFirstDeclarationsOfName =
        savedCurrentScopeFirstDeclarationsOfName;
    }
    currentLexicalScope = savedCurrentScope;
    currentClassHasParameterProperties =
      savedCurrentClassHasParameterProperties;
    return visited;
  }
  function onBeforeVisitNode(node) {
    switch (node.kind) {
      case SyntaxKind.SourceFile:
      case SyntaxKind.CaseBlock:
      case SyntaxKind.ModuleBlock:
      case SyntaxKind.Block:
        currentLexicalScope = node;
        currentScopeFirstDeclarationsOfName = undefined;
        break;
      case SyntaxKind.ClassDeclaration:
      case SyntaxKind.FunctionDeclaration:
        if (hasSyntacticModifier(node, ModifierFlags.Ambient)) {
          break;
        }
        if (node.name) {
          recordEmittedDeclarationInScope(node);
        } else {
          Debug.assert(
            node.kind === SyntaxKind.ClassDeclaration ||
              hasSyntacticModifier(node, ModifierFlags.Default),
          );
        }
        break;
    }
  }
  function visitor(node) {
    return saveStateAndInvoke(node, visitorWorker);
  }
  function visitorWorker(node) {
    if (node.transformFlags & TransformFlags.ContainsTypeScript) {
      return visitTypeScript(node);
    }
    return node;
  }
  function sourceElementVisitor(node) {
    return saveStateAndInvoke(node, sourceElementVisitorWorker);
  }
  function sourceElementVisitorWorker(node) {
    switch (node.kind) {
      case SyntaxKind.ImportDeclaration:
      case SyntaxKind.ImportEqualsDeclaration:
      case SyntaxKind.ExportAssignment:
      case SyntaxKind.ExportDeclaration:
        return visitElidableStatement(node);
      default:
        return visitorWorker(node);
    }
  }
  function isElisionBlocked(node) {
    const parsed = getParseTreeNode(node);
    if (parsed === node || isExportAssignment(node)) {
      return false;
    }
    if (!parsed || parsed.kind !== node.kind) {
      return true;
    }
    switch (node.kind) {
      case SyntaxKind.ImportDeclaration:
        Debug.assertNode(parsed, isImportDeclaration);
        if (node.importClause !== parsed.importClause) {
          return true;
        }
        if (node.attributes !== parsed.attributes) {
          return true;
        }
        break;
      case SyntaxKind.ImportEqualsDeclaration:
        Debug.assertNode(parsed, isImportEqualsDeclaration);
        if (node.name !== parsed.name) {
          return true;
        }
        if (node.isTypeOnly !== parsed.isTypeOnly) {
          return true;
        }
        if (
          node.moduleReference !== parsed.moduleReference &&
          (isEntityName(node.moduleReference) ||
            isEntityName(parsed.moduleReference))
        ) {
          return true;
        }
        break;
      case SyntaxKind.ExportDeclaration:
        Debug.assertNode(parsed, isExportDeclaration);
        if (node.exportClause !== parsed.exportClause) {
          return true;
        }
        if (node.attributes !== parsed.attributes) {
          return true;
        }
        break;
    }
    return false;
  }
  function visitElidableStatement(node) {
    if (isElisionBlocked(node)) {
      if (node.transformFlags & TransformFlags.ContainsTypeScript) {
        return visitEachChild(node, visitor, context);
      }
      return node;
    }
    switch (node.kind) {
      case SyntaxKind.ImportDeclaration:
        return visitImportDeclaration(node);
      case SyntaxKind.ImportEqualsDeclaration:
        return visitImportEqualsDeclaration(node);
      case SyntaxKind.ExportAssignment:
        return visitExportAssignment(node);
      case SyntaxKind.ExportDeclaration:
        return visitExportDeclaration(node);
      default:
        Debug.fail("Unhandled ellided statement");
    }
  }
  function namespaceElementVisitor(node) {
    return saveStateAndInvoke(node, namespaceElementVisitorWorker);
  }
  function namespaceElementVisitorWorker(node) {
    if (
      node.kind === SyntaxKind.ExportDeclaration ||
      node.kind === SyntaxKind.ImportDeclaration ||
      node.kind === SyntaxKind.ImportClause ||
      node.kind === SyntaxKind.ImportEqualsDeclaration &&
        node.moduleReference.kind === SyntaxKind.ExternalModuleReference
    ) {
      return undefined;
    } else if (
      node.transformFlags & TransformFlags.ContainsTypeScript ||
      hasSyntacticModifier(node, ModifierFlags.Export)
    ) {
      return visitTypeScript(node);
    }
    return node;
  }
  function getClassElementVisitor(parent) {
    return (node) =>
      saveStateAndInvoke(node, (n) => classElementVisitorWorker(n, parent));
  }
  function classElementVisitorWorker(node, parent) {
    switch (node.kind) {
      case SyntaxKind.Constructor:
        return visitConstructor(node);
      case SyntaxKind.PropertyDeclaration:
        return visitPropertyDeclaration(node, parent);
      case SyntaxKind.GetAccessor:
        return visitGetAccessor(node, parent);
      case SyntaxKind.SetAccessor:
        return visitSetAccessor(node, parent);
      case SyntaxKind.MethodDeclaration:
        return visitMethodDeclaration(node, parent);
      case SyntaxKind.ClassStaticBlockDeclaration:
        return visitEachChild(node, visitor, context);
      case SyntaxKind.SemicolonClassElement:
        return node;
      case SyntaxKind.IndexSignature:
        return;
      default:
        return Debug.failBadSyntaxKind(node);
    }
  }
  function getObjectLiteralElementVisitor(parent) {
    return (node) =>
      saveStateAndInvoke(
        node,
        (n) => objectLiteralElementVisitorWorker(n, parent),
      );
  }
  function objectLiteralElementVisitorWorker(node, parent) {
    switch (node.kind) {
      case SyntaxKind.PropertyAssignment:
      case SyntaxKind.ShorthandPropertyAssignment:
      case SyntaxKind.SpreadAssignment:
        return visitor(node);
      case SyntaxKind.GetAccessor:
        return visitGetAccessor(node, parent);
      case SyntaxKind.SetAccessor:
        return visitSetAccessor(node, parent);
      case SyntaxKind.MethodDeclaration:
        return visitMethodDeclaration(node, parent);
      default:
        return Debug.failBadSyntaxKind(node);
    }
  }
  function decoratorElidingVisitor(node) {
    return isDecorator(node) ? undefined : visitor(node);
  }
  function modifierElidingVisitor(node) {
    return isModifier(node) ? undefined : visitor(node);
  }
  function modifierVisitor(node) {
    if (isDecorator(node)) return undefined;
    if (modifierToFlag(node.kind) & ModifierFlags.TypeScriptModifier) {
      return undefined;
    } else if (currentNamespace && node.kind === SyntaxKind.ExportKeyword) {
      return undefined;
    }
    return node;
  }
  function visitTypeScript(node) {
    if (
      isStatement(node) && hasSyntacticModifier(node, ModifierFlags.Ambient)
    ) {
      return factory.createNotEmittedStatement(node);
    }
    switch (node.kind) {
      case SyntaxKind.ExportKeyword:
      case SyntaxKind.DefaultKeyword:
        return currentNamespace ? undefined : node;
      case SyntaxKind.PublicKeyword:
      case SyntaxKind.PrivateKeyword:
      case SyntaxKind.ProtectedKeyword:
      case SyntaxKind.AbstractKeyword:
      case SyntaxKind.OverrideKeyword:
      case SyntaxKind.ConstKeyword:
      case SyntaxKind.DeclareKeyword:
      case SyntaxKind.ReadonlyKeyword:
      case SyntaxKind.InKeyword:
      case SyntaxKind.OutKeyword:
      case SyntaxKind.ArrayType:
      case SyntaxKind.TupleType:
      case SyntaxKind.OptionalType:
      case SyntaxKind.RestType:
      case SyntaxKind.TypeLiteral:
      case SyntaxKind.TypePredicate:
      case SyntaxKind.TypeParameter:
      case SyntaxKind.AnyKeyword:
      case SyntaxKind.UnknownKeyword:
      case SyntaxKind.BooleanKeyword:
      case SyntaxKind.StringKeyword:
      case SyntaxKind.NumberKeyword:
      case SyntaxKind.NeverKeyword:
      case SyntaxKind.VoidKeyword:
      case SyntaxKind.SymbolKeyword:
      case SyntaxKind.ConstructorType:
      case SyntaxKind.FunctionType:
      case SyntaxKind.TypeQuery:
      case SyntaxKind.TypeReference:
      case SyntaxKind.UnionType:
      case SyntaxKind.IntersectionType:
      case SyntaxKind.ConditionalType:
      case SyntaxKind.ParenthesizedType:
      case SyntaxKind.ThisType:
      case SyntaxKind.TypeOperator:
      case SyntaxKind.IndexedAccessType:
      case SyntaxKind.MappedType:
      case SyntaxKind.LiteralType:
      case SyntaxKind.IndexSignature:
        return undefined;
      case SyntaxKind.TypeAliasDeclaration:
        return factory.createNotEmittedStatement(node);
      case SyntaxKind.NamespaceExportDeclaration:
        return undefined;
      case SyntaxKind.InterfaceDeclaration:
        return factory.createNotEmittedStatement(node);
      case SyntaxKind.ClassDeclaration:
        return visitClassDeclaration(node);
      case SyntaxKind.ClassExpression:
        return visitClassExpression(node);
      case SyntaxKind.HeritageClause:
        return visitHeritageClause(node);
      case SyntaxKind.ExpressionWithTypeArguments:
        return visitExpressionWithTypeArguments(node);
      case SyntaxKind.ObjectLiteralExpression:
        return visitObjectLiteralExpression(node);
      case SyntaxKind.Constructor:
      case SyntaxKind.PropertyDeclaration:
      case SyntaxKind.MethodDeclaration:
      case SyntaxKind.GetAccessor:
      case SyntaxKind.SetAccessor:
      case SyntaxKind.ClassStaticBlockDeclaration:
        return Debug.fail(
          "Class and object literal elements must be visited with their respective visitors",
        );
      case SyntaxKind.FunctionDeclaration:
        return visitFunctionDeclaration(node);
      case SyntaxKind.FunctionExpression:
        return visitFunctionExpression(node);
      case SyntaxKind.ArrowFunction:
        return visitArrowFunction(node);
      case SyntaxKind.Parameter:
        return visitParameter(node);
      case SyntaxKind.ParenthesizedExpression:
        return visitParenthesizedExpression(node);
      case SyntaxKind.TypeAssertionExpression:
      case SyntaxKind.AsExpression:
        return visitAssertionExpression(node);
      case SyntaxKind.SatisfiesExpression:
        return visitSatisfiesExpression(node);
      case SyntaxKind.CallExpression:
        return visitCallExpression(node);
      case SyntaxKind.NewExpression:
        return visitNewExpression(node);
      case SyntaxKind.TaggedTemplateExpression:
        return visitTaggedTemplateExpression(node);
      case SyntaxKind.NonNullExpression:
        return visitNonNullExpression(node);
      case SyntaxKind.EnumDeclaration:
        return visitEnumDeclaration(node);
      case SyntaxKind.VariableStatement:
        return visitVariableStatement(node);
      case SyntaxKind.VariableDeclaration:
        return visitVariableDeclaration(node);
      case SyntaxKind.ModuleDeclaration:
        return visitModuleDeclaration(node);
      case SyntaxKind.ImportEqualsDeclaration:
        return visitImportEqualsDeclaration(node);
      case SyntaxKind.JsxSelfClosingElement:
        return visitJsxSelfClosingElement(node);
      case SyntaxKind.JsxOpeningElement:
        return visitJsxJsxOpeningElement(node);
      default:
        return visitEachChild(node, visitor, context);
    }
  }
  function visitSourceFile(node) {
    const alwaysStrict =
      getStrictOptionValue(compilerOptions, "alwaysStrict") &&
      !(isExternalModule(node) && moduleKind >= ModuleKind.ES2015) &&
      !isJsonSourceFile(node);
    return factory.updateSourceFile(
      node,
      visitLexicalEnvironment(
        node.statements,
        sourceElementVisitor,
        context,
        0,
        alwaysStrict,
      ),
    );
  }
  function visitObjectLiteralExpression(node) {
    return factory.updateObjectLiteralExpression(
      node,
      visitNodes(
        node.properties,
        getObjectLiteralElementVisitor(node),
        isObjectLiteralElementLike,
      ),
    );
  }
  function getClassFacts(node) {
    let facts = ClassFacts.None;
    if (some(getProperties(node, true, true))) {
      facts |= ClassFacts.HasStaticInitializedProperties;
    }
    const extendsClauseElement = getEffectiveBaseTypeNode(node);
    if (
      extendsClauseElement &&
      skipOuterExpressions(extendsClauseElement.expression).kind !==
        SyntaxKind.NullKeyword
    ) facts |= ClassFacts.IsDerivedClass;
    if (classOrConstructorParameterIsDecorated(legacyDecorators, node)) {
      facts |= ClassFacts.HasClassOrConstructorParameterDecorators;
    }
    if (childIsDecorated(legacyDecorators, node)) {
      facts |= ClassFacts.HasMemberDecorators;
    }
    if (isExportOfNamespace(node)) facts |= ClassFacts.IsExportOfNamespace;
    else if (isDefaultExternalModuleExport(node)) {
      facts |= ClassFacts.IsDefaultExternalExport;
    } else if (isNamedExternalModuleExport(node)) {
      facts |= ClassFacts.IsNamedExternalExport;
    }
    return facts;
  }
  function hasTypeScriptClassSyntax(node) {
    return !!(node.transformFlags &
      TransformFlags.ContainsTypeScriptClassSyntax);
  }
  function isClassLikeDeclarationWithTypeScriptSyntax(node) {
    return hasDecorators(node) || some(node.typeParameters) ||
      some(node.heritageClauses, hasTypeScriptClassSyntax) ||
      some(node.members, hasTypeScriptClassSyntax);
  }
  function visitClassDeclaration(node) {
    const facts = getClassFacts(node);
    const promoteToIIFE = languageVersion <= ScriptTarget.ES5 &&
      !!(facts & ClassFacts.MayNeedImmediatelyInvokedFunctionExpression);
    if (
      !isClassLikeDeclarationWithTypeScriptSyntax(node) &&
      !classOrConstructorParameterIsDecorated(legacyDecorators, node) &&
      !isExportOfNamespace(node)
    ) {
      return factory.updateClassDeclaration(
        node,
        visitNodes(node.modifiers, modifierVisitor, isModifier),
        node.name,
        undefined,
        visitNodes(node.heritageClauses, visitor, isHeritageClause),
        visitNodes(node.members, getClassElementVisitor(node), isClassElement),
      );
    }
    if (promoteToIIFE) {
      context.startLexicalEnvironment();
    }
    const moveModifiers = promoteToIIFE ||
      facts & ClassFacts.IsExportOfNamespace;
    let modifiers = moveModifiers
      ? visitNodes(node.modifiers, modifierElidingVisitor, isModifierLike)
      : visitNodes(node.modifiers, visitor, isModifierLike);
    if (facts & ClassFacts.HasClassOrConstructorParameterDecorators) {
      modifiers = injectClassTypeMetadata(modifiers, node);
    }
    const needsName = moveModifiers && !node.name ||
      facts & ClassFacts.HasMemberDecorators ||
      facts & ClassFacts.HasStaticInitializedProperties;
    const name = needsName
      ? node.name ?? factory.getGeneratedNameForNode(node)
      : node.name;
    const classDeclaration = factory.updateClassDeclaration(
      node,
      modifiers,
      name,
      undefined,
      visitNodes(node.heritageClauses, visitor, isHeritageClause),
      transformClassMembers(node),
    );
    let emitFlags = getEmitFlags(node);
    if (facts & ClassFacts.HasStaticInitializedProperties) {
      emitFlags |= EmitFlags.NoTrailingSourceMap;
    }
    setEmitFlags(classDeclaration, emitFlags);
    let statement;
    if (promoteToIIFE) {
      const statements = [
        classDeclaration,
      ];
      const closingBraceLocation = createTokenRange(
        skipTrivia(currentSourceFile.text, node.members.end),
        SyntaxKind.CloseBraceToken,
      );
      const localName = factory.getInternalName(node);
      const outer = factory.createPartiallyEmittedExpression(localName);
      setTextRangeEnd(outer, closingBraceLocation.end);
      setEmitFlags(outer, EmitFlags.NoComments);
      const returnStatement = factory.createReturnStatement(outer);
      setTextRangePos(returnStatement, closingBraceLocation.pos);
      setEmitFlags(
        returnStatement,
        EmitFlags.NoComments | EmitFlags.NoTokenSourceMaps,
      );
      statements.push(returnStatement);
      insertStatementsAfterStandardPrologue(
        statements,
        context.endLexicalEnvironment(),
      );
      const iife = factory.createImmediatelyInvokedArrowFunction(statements);
      setInternalEmitFlags(iife, InternalEmitFlags.TypeScriptClassWrapper);
      const varDecl = factory.createVariableDeclaration(
        factory.getLocalName(node, false, false),
        undefined,
        undefined,
        iife,
      );
      setOriginalNode(varDecl, node);
      const varStatement = factory.createVariableStatement(
        undefined,
        factory.createVariableDeclarationList([
          varDecl,
        ], NodeFlags.Let),
      );
      setOriginalNode(varStatement, node);
      setCommentRange(varStatement, node);
      setSourceMapRange(varStatement, moveRangePastDecorators(node));
      startOnNewLine(varStatement);
      statement = varStatement;
    } else {
      statement = classDeclaration;
    }
    if (moveModifiers) {
      if (facts & ClassFacts.IsExportOfNamespace) {
        return [
          statement,
          createExportMemberAssignmentStatement(node),
        ];
      }
      if (facts & ClassFacts.IsDefaultExternalExport) {
        return [
          statement,
          factory.createExportDefault(factory.getLocalName(node, false, true)),
        ];
      }
      if (facts & ClassFacts.IsNamedExternalExport) {
        return [
          statement,
          factory.createExternalModuleExport(
            factory.getDeclarationName(node, false, true),
          ),
        ];
      }
    }
    return statement;
  }
  function visitClassExpression(node) {
    let modifiers = visitNodes(
      node.modifiers,
      modifierElidingVisitor,
      isModifierLike,
    );
    if (classOrConstructorParameterIsDecorated(legacyDecorators, node)) {
      modifiers = injectClassTypeMetadata(modifiers, node);
    }
    return factory.updateClassExpression(
      node,
      modifiers,
      node.name,
      undefined,
      visitNodes(node.heritageClauses, visitor, isHeritageClause),
      transformClassMembers(node),
    );
  }
  function transformClassMembers(node) {
    const members = visitNodes(
      node.members,
      getClassElementVisitor(node),
      isClassElement,
    );
    let newMembers;
    const constructor = getFirstConstructorWithBody(node);
    const parametersWithPropertyAssignments = constructor &&
      filter(
        constructor.parameters,
        (p) => isParameterPropertyDeclaration(p, constructor),
      );
    if (parametersWithPropertyAssignments) {
      for (const parameter of parametersWithPropertyAssignments) {
        const parameterProperty = factory.createPropertyDeclaration(
          undefined,
          parameter.name,
          undefined,
          undefined,
          undefined,
        );
        setOriginalNode(parameterProperty, parameter);
        newMembers = append(newMembers, parameterProperty);
      }
    }
    if (newMembers) {
      newMembers = addRange(newMembers, members);
      return setTextRange(factory.createNodeArray(newMembers), node.members);
    }
    return members;
  }
  function injectClassTypeMetadata(modifiers, node) {
    const metadata = getTypeMetadata(node, node);
    if (some(metadata)) {
      const modifiersArray = [];
      addRange(modifiersArray, takeWhile(modifiers, isExportOrDefaultModifier));
      addRange(modifiersArray, filter(modifiers, isDecorator));
      addRange(modifiersArray, metadata);
      addRange(
        modifiersArray,
        filter(skipWhile(modifiers, isExportOrDefaultModifier), isModifier),
      );
      modifiers = setTextRange(
        factory.createNodeArray(modifiersArray),
        modifiers,
      );
    }
    return modifiers;
  }
  function injectClassElementTypeMetadata(modifiers, node, container) {
    if (
      isClassLike(container) &&
      classElementOrClassElementParameterIsDecorated(
        legacyDecorators,
        node,
        container,
      )
    ) {
      const metadata = getTypeMetadata(node, container);
      if (some(metadata)) {
        const modifiersArray = [];
        addRange(modifiersArray, filter(modifiers, isDecorator));
        addRange(modifiersArray, metadata);
        addRange(modifiersArray, filter(modifiers, isModifier));
        modifiers = setTextRange(
          factory.createNodeArray(modifiersArray),
          modifiers,
        );
      }
    }
    return modifiers;
  }
  function getTypeMetadata(node, container) {
    if (!legacyDecorators) return undefined;
    return USE_NEW_TYPE_METADATA_FORMAT
      ? getNewTypeMetadata(node, container)
      : getOldTypeMetadata(node, container);
  }
  function getOldTypeMetadata(node, container) {
    if (typeSerializer) {
      let decorators;
      if (shouldAddTypeMetadata(node)) {
        const typeMetadata = emitHelpers().createMetadataHelper(
          "design:type",
          typeSerializer.serializeTypeOfNode(
            {
              currentLexicalScope,
              currentNameScope: container,
            },
            node,
            container,
          ),
        );
        decorators = append(decorators, factory.createDecorator(typeMetadata));
      }
      if (shouldAddParamTypesMetadata(node)) {
        const paramTypesMetadata = emitHelpers().createMetadataHelper(
          "design:paramtypes",
          typeSerializer.serializeParameterTypesOfNode(
            {
              currentLexicalScope,
              currentNameScope: container,
            },
            node,
            container,
          ),
        );
        decorators = append(
          decorators,
          factory.createDecorator(paramTypesMetadata),
        );
      }
      if (shouldAddReturnTypeMetadata(node)) {
        const returnTypeMetadata = emitHelpers().createMetadataHelper(
          "design:returntype",
          typeSerializer.serializeReturnTypeOfNode({
            currentLexicalScope,
            currentNameScope: container,
          }, node),
        );
        decorators = append(
          decorators,
          factory.createDecorator(returnTypeMetadata),
        );
      }
      return decorators;
    }
  }
  function getNewTypeMetadata(node, container) {
    if (typeSerializer) {
      let properties;
      if (shouldAddTypeMetadata(node)) {
        const typeProperty = factory.createPropertyAssignment(
          "type",
          factory.createArrowFunction(
            undefined,
            undefined,
            [],
            undefined,
            factory.createToken(SyntaxKind.EqualsGreaterThanToken),
            typeSerializer.serializeTypeOfNode(
              {
                currentLexicalScope,
                currentNameScope: container,
              },
              node,
              container,
            ),
          ),
        );
        properties = append(properties, typeProperty);
      }
      if (shouldAddParamTypesMetadata(node)) {
        const paramTypeProperty = factory.createPropertyAssignment(
          "paramTypes",
          factory.createArrowFunction(
            undefined,
            undefined,
            [],
            undefined,
            factory.createToken(SyntaxKind.EqualsGreaterThanToken),
            typeSerializer.serializeParameterTypesOfNode(
              {
                currentLexicalScope,
                currentNameScope: container,
              },
              node,
              container,
            ),
          ),
        );
        properties = append(properties, paramTypeProperty);
      }
      if (shouldAddReturnTypeMetadata(node)) {
        const returnTypeProperty = factory.createPropertyAssignment(
          "returnType",
          factory.createArrowFunction(
            undefined,
            undefined,
            [],
            undefined,
            factory.createToken(SyntaxKind.EqualsGreaterThanToken),
            typeSerializer.serializeReturnTypeOfNode({
              currentLexicalScope,
              currentNameScope: container,
            }, node),
          ),
        );
        properties = append(properties, returnTypeProperty);
      }
      if (properties) {
        const typeInfoMetadata = emitHelpers().createMetadataHelper(
          "design:typeinfo",
          factory.createObjectLiteralExpression(properties, true),
        );
        return [
          factory.createDecorator(typeInfoMetadata),
        ];
      }
    }
  }
  function shouldAddTypeMetadata(node) {
    const kind = node.kind;
    return kind === SyntaxKind.MethodDeclaration ||
      kind === SyntaxKind.GetAccessor || kind === SyntaxKind.SetAccessor ||
      kind === SyntaxKind.PropertyDeclaration;
  }
  function shouldAddReturnTypeMetadata(node) {
    return node.kind === SyntaxKind.MethodDeclaration;
  }
  function shouldAddParamTypesMetadata(node) {
    switch (node.kind) {
      case SyntaxKind.ClassDeclaration:
      case SyntaxKind.ClassExpression:
        return getFirstConstructorWithBody(node) !== undefined;
      case SyntaxKind.MethodDeclaration:
      case SyntaxKind.GetAccessor:
      case SyntaxKind.SetAccessor:
        return true;
    }
    return false;
  }
  function getExpressionForPropertyName(
    member,
    generateNameForComputedPropertyName,
  ) {
    const name = member.name;
    if (isPrivateIdentifier(name)) {
      return factory.createIdentifier("");
    } else if (isComputedPropertyName(name)) {
      return generateNameForComputedPropertyName &&
          !isSimpleInlineableExpression(name.expression)
        ? factory.getGeneratedNameForNode(name)
        : name.expression;
    } else if (isIdentifier(name)) {
      return factory.createStringLiteral(idText(name));
    } else {
      return factory.cloneNode(name);
    }
  }
  function visitPropertyNameOfClassElement(member) {
    const name = member.name;
    if (
      isComputedPropertyName(name) &&
      (!hasStaticModifier(member) && currentClassHasParameterProperties ||
        hasDecorators(member) && legacyDecorators)
    ) {
      const expression = visitNode(name.expression, visitor, isExpression);
      Debug.assert(expression);
      const innerExpression = skipPartiallyEmittedExpressions(expression);
      if (!isSimpleInlineableExpression(innerExpression)) {
        const generatedName = factory.getGeneratedNameForNode(name);
        hoistVariableDeclaration(generatedName);
        return factory.updateComputedPropertyName(
          name,
          factory.createAssignment(generatedName, expression),
        );
      }
    }
    return Debug.checkDefined(visitNode(name, visitor, isPropertyName));
  }
  function visitHeritageClause(node) {
    if (node.token === SyntaxKind.ImplementsKeyword) {
      return undefined;
    }
    return visitEachChild(node, visitor, context);
  }
  function visitExpressionWithTypeArguments(node) {
    return factory.updateExpressionWithTypeArguments(
      node,
      Debug.checkDefined(
        visitNode(node.expression, visitor, isLeftHandSideExpression),
      ),
      undefined,
    );
  }
  function shouldEmitFunctionLikeDeclaration(node) {
    return !nodeIsMissing(node.body);
  }
  function visitPropertyDeclaration(node, parent) {
    const isAmbient = node.flags & NodeFlags.Ambient ||
      hasSyntacticModifier(node, ModifierFlags.Abstract);
    if (isAmbient && !(legacyDecorators && hasDecorators(node))) {
      return undefined;
    }
    let modifiers = isClassLike(parent)
      ? !isAmbient
        ? visitNodes(node.modifiers, visitor, isModifierLike)
        : visitNodes(node.modifiers, modifierElidingVisitor, isModifierLike)
      : visitNodes(node.modifiers, decoratorElidingVisitor, isModifierLike);
    modifiers = injectClassElementTypeMetadata(modifiers, node, parent);
    if (isAmbient) {
      return factory.updatePropertyDeclaration(
        node,
        concatenate(
          modifiers,
          factory.createModifiersFromModifierFlags(ModifierFlags.Ambient),
        ),
        Debug.checkDefined(visitNode(node.name, visitor, isPropertyName)),
        undefined,
        undefined,
        undefined,
      );
    }
    return factory.updatePropertyDeclaration(
      node,
      modifiers,
      visitPropertyNameOfClassElement(node),
      undefined,
      undefined,
      visitNode(node.initializer, visitor, isExpression),
    );
  }
  function visitConstructor(node) {
    if (!shouldEmitFunctionLikeDeclaration(node)) {
      return undefined;
    }
    return factory.updateConstructorDeclaration(
      node,
      undefined,
      visitParameterList(node.parameters, visitor, context),
      transformConstructorBody(node.body, node),
    );
  }
  function transformConstructorBodyWorker(
    statementsOut,
    statementsIn,
    statementOffset,
    superPath,
    superPathDepth,
    initializerStatements,
  ) {
    const superStatementIndex = superPath[superPathDepth];
    const superStatement = statementsIn[superStatementIndex];
    addRange(
      statementsOut,
      visitNodes(
        statementsIn,
        visitor,
        isStatement,
        statementOffset,
        superStatementIndex - statementOffset,
      ),
    );
    if (isTryStatement(superStatement)) {
      const tryBlockStatements = [];
      transformConstructorBodyWorker(
        tryBlockStatements,
        superStatement.tryBlock.statements,
        0,
        superPath,
        superPathDepth + 1,
        initializerStatements,
      );
      const tryBlockStatementsArray = factory.createNodeArray(
        tryBlockStatements,
      );
      setTextRange(tryBlockStatementsArray, superStatement.tryBlock.statements);
      statementsOut.push(
        factory.updateTryStatement(
          superStatement,
          factory.updateBlock(superStatement.tryBlock, tryBlockStatements),
          visitNode(superStatement.catchClause, visitor, isCatchClause),
          visitNode(superStatement.finallyBlock, visitor, isBlock),
        ),
      );
    } else {
      addRange(
        statementsOut,
        visitNodes(statementsIn, visitor, isStatement, superStatementIndex, 1),
      );
      addRange(statementsOut, initializerStatements);
    }
    addRange(
      statementsOut,
      visitNodes(statementsIn, visitor, isStatement, superStatementIndex + 1),
    );
  }
  function transformConstructorBody(body, constructor) {
    const parametersWithPropertyAssignments = constructor &&
      filter(
        constructor.parameters,
        (p) => isParameterPropertyDeclaration(p, constructor),
      );
    if (!some(parametersWithPropertyAssignments)) {
      return visitFunctionBody(body, visitor, context);
    }
    let statements = [];
    resumeLexicalEnvironment();
    const prologueStatementCount = factory.copyPrologue(
      body.statements,
      statements,
      false,
      visitor,
    );
    const superPath = findSuperStatementIndexPath(
      body.statements,
      prologueStatementCount,
    );
    const parameterPropertyAssignments = mapDefined(
      parametersWithPropertyAssignments,
      transformParameterWithPropertyAssignment,
    );
    if (superPath.length) {
      transformConstructorBodyWorker(
        statements,
        body.statements,
        prologueStatementCount,
        superPath,
        0,
        parameterPropertyAssignments,
      );
    } else {
      addRange(statements, parameterPropertyAssignments);
      addRange(
        statements,
        visitNodes(
          body.statements,
          visitor,
          isStatement,
          prologueStatementCount,
        ),
      );
    }
    statements = factory.mergeLexicalEnvironment(
      statements,
      endLexicalEnvironment(),
    );
    const block = factory.createBlock(
      setTextRange(factory.createNodeArray(statements), body.statements),
      true,
    );
    setTextRange(block, body);
    setOriginalNode(block, body);
    return block;
  }
  function transformParameterWithPropertyAssignment(node) {
    const name = node.name;
    if (!isIdentifier(name)) {
      return undefined;
    }
    const propertyName = setParent(
      setTextRange(factory.cloneNode(name), name),
      name.parent,
    );
    setEmitFlags(propertyName, EmitFlags.NoComments | EmitFlags.NoSourceMap);
    const localName = setParent(
      setTextRange(factory.cloneNode(name), name),
      name.parent,
    );
    setEmitFlags(localName, EmitFlags.NoComments);
    return startOnNewLine(
      removeAllComments(
        setTextRange(
          setOriginalNode(
            factory.createExpressionStatement(
              factory.createAssignment(
                setTextRange(
                  factory.createPropertyAccessExpression(
                    factory.createThis(),
                    propertyName,
                  ),
                  node.name,
                ),
                localName,
              ),
            ),
            node,
          ),
          moveRangePos(node, -1),
        ),
      ),
    );
  }
  function visitMethodDeclaration(node, parent) {
    if (!(node.transformFlags & TransformFlags.ContainsTypeScript)) {
      return node;
    }
    if (!shouldEmitFunctionLikeDeclaration(node)) {
      return undefined;
    }
    let modifiers = isClassLike(parent)
      ? visitNodes(node.modifiers, visitor, isModifierLike)
      : visitNodes(node.modifiers, decoratorElidingVisitor, isModifierLike);
    modifiers = injectClassElementTypeMetadata(modifiers, node, parent);
    return factory.updateMethodDeclaration(
      node,
      modifiers,
      node.asteriskToken,
      visitPropertyNameOfClassElement(node),
      undefined,
      undefined,
      visitParameterList(node.parameters, visitor, context),
      undefined,
      visitFunctionBody(node.body, visitor, context),
    );
  }
  function shouldEmitAccessorDeclaration(node) {
    return !(nodeIsMissing(node.body) &&
      hasSyntacticModifier(node, ModifierFlags.Abstract));
  }
  function visitGetAccessor(node, parent) {
    if (!(node.transformFlags & TransformFlags.ContainsTypeScript)) {
      return node;
    }
    if (!shouldEmitAccessorDeclaration(node)) {
      return undefined;
    }
    let modifiers = isClassLike(parent)
      ? visitNodes(node.modifiers, visitor, isModifierLike)
      : visitNodes(node.modifiers, decoratorElidingVisitor, isModifierLike);
    modifiers = injectClassElementTypeMetadata(modifiers, node, parent);
    return factory.updateGetAccessorDeclaration(
      node,
      modifiers,
      visitPropertyNameOfClassElement(node),
      visitParameterList(node.parameters, visitor, context),
      undefined,
      visitFunctionBody(node.body, visitor, context) || factory.createBlock([]),
    );
  }
  function visitSetAccessor(node, parent) {
    if (!(node.transformFlags & TransformFlags.ContainsTypeScript)) {
      return node;
    }
    if (!shouldEmitAccessorDeclaration(node)) {
      return undefined;
    }
    let modifiers = isClassLike(parent)
      ? visitNodes(node.modifiers, visitor, isModifierLike)
      : visitNodes(node.modifiers, decoratorElidingVisitor, isModifierLike);
    modifiers = injectClassElementTypeMetadata(modifiers, node, parent);
    return factory.updateSetAccessorDeclaration(
      node,
      modifiers,
      visitPropertyNameOfClassElement(node),
      visitParameterList(node.parameters, visitor, context),
      visitFunctionBody(node.body, visitor, context) || factory.createBlock([]),
    );
  }
  function visitFunctionDeclaration(node) {
    if (!shouldEmitFunctionLikeDeclaration(node)) {
      return factory.createNotEmittedStatement(node);
    }
    const updated = factory.updateFunctionDeclaration(
      node,
      visitNodes(node.modifiers, modifierVisitor, isModifier),
      node.asteriskToken,
      node.name,
      undefined,
      visitParameterList(node.parameters, visitor, context),
      undefined,
      visitFunctionBody(node.body, visitor, context) || factory.createBlock([]),
    );
    if (isExportOfNamespace(node)) {
      const statements = [
        updated,
      ];
      addExportMemberAssignment(statements, node);
      return statements;
    }
    return updated;
  }
  function visitFunctionExpression(node) {
    if (!shouldEmitFunctionLikeDeclaration(node)) {
      return factory.createOmittedExpression();
    }
    const updated = factory.updateFunctionExpression(
      node,
      visitNodes(node.modifiers, modifierVisitor, isModifier),
      node.asteriskToken,
      node.name,
      undefined,
      visitParameterList(node.parameters, visitor, context),
      undefined,
      visitFunctionBody(node.body, visitor, context) || factory.createBlock([]),
    );
    return updated;
  }
  function visitArrowFunction(node) {
    const updated = factory.updateArrowFunction(
      node,
      visitNodes(node.modifiers, modifierVisitor, isModifier),
      undefined,
      visitParameterList(node.parameters, visitor, context),
      undefined,
      node.equalsGreaterThanToken,
      visitFunctionBody(node.body, visitor, context),
    );
    return updated;
  }
  function visitParameter(node) {
    if (parameterIsThisKeyword(node)) {
      return undefined;
    }
    const updated = factory.updateParameterDeclaration(
      node,
      visitNodes(
        node.modifiers,
        (node) => isDecorator(node) ? visitor(node) : undefined,
        isModifierLike,
      ),
      node.dotDotDotToken,
      Debug.checkDefined(visitNode(node.name, visitor, isBindingName)),
      undefined,
      undefined,
      visitNode(node.initializer, visitor, isExpression),
    );
    if (updated !== node) {
      setCommentRange(updated, node);
      setTextRange(updated, moveRangePastModifiers(node));
      setSourceMapRange(updated, moveRangePastModifiers(node));
      setEmitFlags(updated.name, EmitFlags.NoTrailingSourceMap);
    }
    return updated;
  }
  function visitVariableStatement(node) {
    if (isExportOfNamespace(node)) {
      const variables = getInitializedVariables(node.declarationList);
      if (variables.length === 0) {
        return undefined;
      }
      return setTextRange(
        factory.createExpressionStatement(
          factory.inlineExpressions(
            map(variables, transformInitializedVariable),
          ),
        ),
        node,
      );
    } else {
      return visitEachChild(node, visitor, context);
    }
  }
  function transformInitializedVariable(node) {
    const name = node.name;
    if (isBindingPattern(name)) {
      return flattenDestructuringAssignment(
        node,
        visitor,
        context,
        FlattenLevel.All,
        false,
        createNamespaceExportExpression,
      );
    } else {
      return setTextRange(
        factory.createAssignment(
          getNamespaceMemberNameWithSourceMapsAndWithoutComments(name),
          Debug.checkDefined(
            visitNode(node.initializer, visitor, isExpression),
          ),
        ),
        node,
      );
    }
  }
  function visitVariableDeclaration(node) {
    const updated = factory.updateVariableDeclaration(
      node,
      Debug.checkDefined(visitNode(node.name, visitor, isBindingName)),
      undefined,
      undefined,
      visitNode(node.initializer, visitor, isExpression),
    );
    if (node.type) {
      setTypeNode(updated.name, node.type);
    }
    return updated;
  }
  function visitParenthesizedExpression(node) {
    const innerExpression = skipOuterExpressions(
      node.expression,
      ~OuterExpressionKinds.Assertions,
    );
    if (isAssertionExpression(innerExpression)) {
      const expression = visitNode(node.expression, visitor, isExpression);
      Debug.assert(expression);
      return factory.createPartiallyEmittedExpression(expression, node);
    }
    return visitEachChild(node, visitor, context);
  }
  function visitAssertionExpression(node) {
    const expression = visitNode(node.expression, visitor, isExpression);
    Debug.assert(expression);
    return factory.createPartiallyEmittedExpression(expression, node);
  }
  function visitNonNullExpression(node) {
    const expression = visitNode(
      node.expression,
      visitor,
      isLeftHandSideExpression,
    );
    Debug.assert(expression);
    return factory.createPartiallyEmittedExpression(expression, node);
  }
  function visitSatisfiesExpression(node) {
    const expression = visitNode(node.expression, visitor, isExpression);
    Debug.assert(expression);
    return factory.createPartiallyEmittedExpression(expression, node);
  }
  function visitCallExpression(node) {
    return factory.updateCallExpression(
      node,
      Debug.checkDefined(visitNode(node.expression, visitor, isExpression)),
      undefined,
      visitNodes(node.arguments, visitor, isExpression),
    );
  }
  function visitNewExpression(node) {
    return factory.updateNewExpression(
      node,
      Debug.checkDefined(visitNode(node.expression, visitor, isExpression)),
      undefined,
      visitNodes(node.arguments, visitor, isExpression),
    );
  }
  function visitTaggedTemplateExpression(node) {
    return factory.updateTaggedTemplateExpression(
      node,
      Debug.checkDefined(visitNode(node.tag, visitor, isExpression)),
      undefined,
      Debug.checkDefined(visitNode(node.template, visitor, isTemplateLiteral)),
    );
  }
  function visitJsxSelfClosingElement(node) {
    return factory.updateJsxSelfClosingElement(
      node,
      Debug.checkDefined(
        visitNode(node.tagName, visitor, isJsxTagNameExpression),
      ),
      undefined,
      Debug.checkDefined(visitNode(node.attributes, visitor, isJsxAttributes)),
    );
  }
  function visitJsxJsxOpeningElement(node) {
    return factory.updateJsxOpeningElement(
      node,
      Debug.checkDefined(
        visitNode(node.tagName, visitor, isJsxTagNameExpression),
      ),
      undefined,
      Debug.checkDefined(visitNode(node.attributes, visitor, isJsxAttributes)),
    );
  }
  function shouldEmitEnumDeclaration(node) {
    return !isEnumConst(node) || shouldPreserveConstEnums(compilerOptions);
  }
  function visitEnumDeclaration(node) {
    if (!shouldEmitEnumDeclaration(node)) {
      return factory.createNotEmittedStatement(node);
    }
    const statements = [];
    let emitFlags = EmitFlags.AdviseOnEmitNode;
    const varAdded = addVarForEnumOrModuleDeclaration(statements, node);
    if (varAdded) {
      if (
        moduleKind !== ModuleKind.System ||
        currentLexicalScope !== currentSourceFile
      ) {
        emitFlags |= EmitFlags.NoLeadingComments;
      }
    }
    const parameterName = getNamespaceParameterName(node);
    const containerName = getNamespaceContainerName(node);
    const exportName = isExportOfNamespace(node)
      ? factory.getExternalModuleOrNamespaceExportName(
        currentNamespaceContainerName,
        node,
        false,
        true,
      )
      : factory.getDeclarationName(node, false, true);
    let moduleArg = factory.createLogicalOr(
      exportName,
      factory.createAssignment(
        exportName,
        factory.createObjectLiteralExpression(),
      ),
    );
    if (isExportOfNamespace(node)) {
      const localName = factory.getLocalName(node, false, true);
      moduleArg = factory.createAssignment(localName, moduleArg);
    }
    const enumStatement = factory.createExpressionStatement(
      factory.createCallExpression(
        factory.createFunctionExpression(
          undefined,
          undefined,
          undefined,
          undefined,
          [
            factory.createParameterDeclaration(
              undefined,
              undefined,
              parameterName,
            ),
          ],
          undefined,
          transformEnumBody(node, containerName),
        ),
        undefined,
        [
          moduleArg,
        ],
      ),
    );
    setOriginalNode(enumStatement, node);
    if (varAdded) {
      setSyntheticLeadingComments(enumStatement, undefined);
      setSyntheticTrailingComments(enumStatement, undefined);
    }
    setTextRange(enumStatement, node);
    addEmitFlags(enumStatement, emitFlags);
    statements.push(enumStatement);
    return statements;
  }
  function transformEnumBody(node, localName) {
    const savedCurrentNamespaceLocalName = currentNamespaceContainerName;
    currentNamespaceContainerName = localName;
    const statements = [];
    startLexicalEnvironment();
    const members = map(node.members, transformEnumMember);
    insertStatementsAfterStandardPrologue(statements, endLexicalEnvironment());
    addRange(statements, members);
    currentNamespaceContainerName = savedCurrentNamespaceLocalName;
    return factory.createBlock(
      setTextRange(factory.createNodeArray(statements), node.members),
      true,
    );
  }
  function transformEnumMember(member) {
    const name = getExpressionForPropertyName(member, false);
    const evaluated = resolver.getEnumMemberValue(member);
    const valueExpression = transformEnumMemberDeclarationValue(
      member,
      evaluated?.value,
    );
    const innerAssignment = factory.createAssignment(
      factory.createElementAccessExpression(
        currentNamespaceContainerName,
        name,
      ),
      valueExpression,
    );
    const outerAssignment =
      typeof evaluated?.value === "string" || evaluated?.isSyntacticallyString
        ? innerAssignment
        : factory.createAssignment(
          factory.createElementAccessExpression(
            currentNamespaceContainerName,
            innerAssignment,
          ),
          name,
        );
    return setTextRange(
      factory.createExpressionStatement(setTextRange(outerAssignment, member)),
      member,
    );
  }
  function transformEnumMemberDeclarationValue(member, constantValue) {
    if (constantValue !== undefined) {
      return typeof constantValue === "string"
        ? factory.createStringLiteral(constantValue)
        : constantValue < 0
        ? factory.createPrefixUnaryExpression(
          SyntaxKind.MinusToken,
          factory.createNumericLiteral(-constantValue),
        )
        : factory.createNumericLiteral(constantValue);
    } else {
      enableSubstitutionForNonQualifiedEnumMembers();
      if (member.initializer) {
        return Debug.checkDefined(
          visitNode(member.initializer, visitor, isExpression),
        );
      } else {
        return factory.createVoidZero();
      }
    }
  }
  function shouldEmitModuleDeclaration(nodeIn) {
    const node = getParseTreeNode(nodeIn, isModuleDeclaration);
    if (!node) {
      return true;
    }
    return isInstantiatedModule(
      node,
      shouldPreserveConstEnums(compilerOptions),
    );
  }
  function recordEmittedDeclarationInScope(node) {
    if (!currentScopeFirstDeclarationsOfName) {
      currentScopeFirstDeclarationsOfName = new Map();
    }
    const name = declaredNameInScope(node);
    if (!currentScopeFirstDeclarationsOfName.has(name)) {
      currentScopeFirstDeclarationsOfName.set(name, node);
    }
  }
  function isFirstEmittedDeclarationInScope(node) {
    if (currentScopeFirstDeclarationsOfName) {
      const name = declaredNameInScope(node);
      return currentScopeFirstDeclarationsOfName.get(name) === node;
    }
    return true;
  }
  function declaredNameInScope(node) {
    Debug.assertNode(node.name, isIdentifier);
    return node.name.escapedText;
  }
  function addVarForEnumOrModuleDeclaration(statements, node) {
    const varDecl = factory.createVariableDeclaration(
      factory.getLocalName(node, false, true),
    );
    const varFlags = currentLexicalScope.kind === SyntaxKind.SourceFile
      ? NodeFlags.None
      : NodeFlags.Let;
    const statement = factory.createVariableStatement(
      visitNodes(node.modifiers, modifierVisitor, isModifier),
      factory.createVariableDeclarationList([
        varDecl,
      ], varFlags),
    );
    setOriginalNode(varDecl, node);
    setSyntheticLeadingComments(varDecl, undefined);
    setSyntheticTrailingComments(varDecl, undefined);
    setOriginalNode(statement, node);
    recordEmittedDeclarationInScope(node);
    if (isFirstEmittedDeclarationInScope(node)) {
      if (node.kind === SyntaxKind.EnumDeclaration) {
        setSourceMapRange(statement.declarationList, node);
      } else {
        setSourceMapRange(statement, node);
      }
      setCommentRange(statement, node);
      addEmitFlags(statement, EmitFlags.NoTrailingComments);
      statements.push(statement);
      return true;
    }
    return false;
  }
  function visitModuleDeclaration(node) {
    if (!shouldEmitModuleDeclaration(node)) {
      return factory.createNotEmittedStatement(node);
    }
    Debug.assertNode(
      node.name,
      isIdentifier,
      "A TypeScript namespace should have an Identifier name.",
    );
    enableSubstitutionForNamespaceExports();
    const statements = [];
    let emitFlags = EmitFlags.AdviseOnEmitNode;
    const varAdded = addVarForEnumOrModuleDeclaration(statements, node);
    if (varAdded) {
      if (
        moduleKind !== ModuleKind.System ||
        currentLexicalScope !== currentSourceFile
      ) {
        emitFlags |= EmitFlags.NoLeadingComments;
      }
    }
    const parameterName = getNamespaceParameterName(node);
    const containerName = getNamespaceContainerName(node);
    const exportName = isExportOfNamespace(node)
      ? factory.getExternalModuleOrNamespaceExportName(
        currentNamespaceContainerName,
        node,
        false,
        true,
      )
      : factory.getDeclarationName(node, false, true);
    let moduleArg = factory.createLogicalOr(
      exportName,
      factory.createAssignment(
        exportName,
        factory.createObjectLiteralExpression(),
      ),
    );
    if (isExportOfNamespace(node)) {
      const localName = factory.getLocalName(node, false, true);
      moduleArg = factory.createAssignment(localName, moduleArg);
    }
    const moduleStatement = factory.createExpressionStatement(
      factory.createCallExpression(
        factory.createFunctionExpression(
          undefined,
          undefined,
          undefined,
          undefined,
          [
            factory.createParameterDeclaration(
              undefined,
              undefined,
              parameterName,
            ),
          ],
          undefined,
          transformModuleBody(node, containerName),
        ),
        undefined,
        [
          moduleArg,
        ],
      ),
    );
    setOriginalNode(moduleStatement, node);
    if (varAdded) {
      setSyntheticLeadingComments(moduleStatement, undefined);
      setSyntheticTrailingComments(moduleStatement, undefined);
    }
    setTextRange(moduleStatement, node);
    addEmitFlags(moduleStatement, emitFlags);
    statements.push(moduleStatement);
    return statements;
  }
  function transformModuleBody(node, namespaceLocalName) {
    const savedCurrentNamespaceContainerName = currentNamespaceContainerName;
    const savedCurrentNamespace = currentNamespace;
    const savedCurrentScopeFirstDeclarationsOfName =
      currentScopeFirstDeclarationsOfName;
    currentNamespaceContainerName = namespaceLocalName;
    currentNamespace = node;
    currentScopeFirstDeclarationsOfName = undefined;
    const statements = [];
    startLexicalEnvironment();
    let statementsLocation;
    let blockLocation;
    if (node.body) {
      if (node.body.kind === SyntaxKind.ModuleBlock) {
        saveStateAndInvoke(
          node.body,
          (body) =>
            addRange(
              statements,
              visitNodes(body.statements, namespaceElementVisitor, isStatement),
            ),
        );
        statementsLocation = node.body.statements;
        blockLocation = node.body;
      } else {
        const result = visitModuleDeclaration(node.body);
        if (result) {
          if (isArray(result)) {
            addRange(statements, result);
          } else {
            statements.push(result);
          }
        }
        const moduleBlock =
          getInnerMostModuleDeclarationFromDottedModule(node).body;
        statementsLocation = moveRangePos(moduleBlock.statements, -1);
      }
    }
    insertStatementsAfterStandardPrologue(statements, endLexicalEnvironment());
    currentNamespaceContainerName = savedCurrentNamespaceContainerName;
    currentNamespace = savedCurrentNamespace;
    currentScopeFirstDeclarationsOfName =
      savedCurrentScopeFirstDeclarationsOfName;
    const block = factory.createBlock(
      setTextRange(factory.createNodeArray(statements), statementsLocation),
      true,
    );
    setTextRange(block, blockLocation);
    if (!node.body || node.body.kind !== SyntaxKind.ModuleBlock) {
      setEmitFlags(block, getEmitFlags(block) | EmitFlags.NoComments);
    }
    return block;
  }
  function getInnerMostModuleDeclarationFromDottedModule(moduleDeclaration) {
    if (moduleDeclaration.body.kind === SyntaxKind.ModuleDeclaration) {
      const recursiveInnerModule =
        getInnerMostModuleDeclarationFromDottedModule(moduleDeclaration.body);
      return recursiveInnerModule || moduleDeclaration.body;
    }
  }
  function visitImportDeclaration(node) {
    if (!node.importClause) {
      return node;
    }
    if (node.importClause.isTypeOnly) {
      return undefined;
    }
    const importClause = visitNode(
      node.importClause,
      visitImportClause,
      isImportClause,
    );
    return importClause
      ? factory.updateImportDeclaration(
        node,
        undefined,
        importClause,
        node.moduleSpecifier,
        node.attributes,
      )
      : undefined;
  }
  function visitImportClause(node) {
    Debug.assert(!node.isTypeOnly);
    const name = shouldEmitAliasDeclaration(node) ? node.name : undefined;
    const namedBindings = visitNode(
      node.namedBindings,
      visitNamedImportBindings,
      isNamedImportBindings,
    );
    return name || namedBindings
      ? factory.updateImportClause(node, false, name, namedBindings)
      : undefined;
  }
  function visitNamedImportBindings(node) {
    if (node.kind === SyntaxKind.NamespaceImport) {
      return shouldEmitAliasDeclaration(node) ? node : undefined;
    } else {
      const allowEmpty = compilerOptions.verbatimModuleSyntax;
      const elements = visitNodes(
        node.elements,
        visitImportSpecifier,
        isImportSpecifier,
      );
      return allowEmpty || some(elements)
        ? factory.updateNamedImports(node, elements)
        : undefined;
    }
  }
  function visitImportSpecifier(node) {
    return !node.isTypeOnly && shouldEmitAliasDeclaration(node)
      ? node
      : undefined;
  }
  function visitExportAssignment(node) {
    return compilerOptions.verbatimModuleSyntax ||
        resolver.isValueAliasDeclaration(node)
      ? visitEachChild(node, visitor, context)
      : undefined;
  }
  function visitExportDeclaration(node) {
    if (node.isTypeOnly) {
      return undefined;
    }
    if (!node.exportClause || isNamespaceExport(node.exportClause)) {
      return node;
    }
    const allowEmpty = !!compilerOptions.verbatimModuleSyntax;
    const exportClause = visitNode(
      node.exportClause,
      (bindings) => visitNamedExportBindings(bindings, allowEmpty),
      isNamedExportBindings,
    );
    return exportClause
      ? factory.updateExportDeclaration(
        node,
        undefined,
        node.isTypeOnly,
        exportClause,
        node.moduleSpecifier,
        node.attributes,
      )
      : undefined;
  }
  function visitNamedExports(node, allowEmpty) {
    const elements = visitNodes(
      node.elements,
      visitExportSpecifier,
      isExportSpecifier,
    );
    return allowEmpty || some(elements)
      ? factory.updateNamedExports(node, elements)
      : undefined;
  }
  function visitNamespaceExports(node) {
    return factory.updateNamespaceExport(
      node,
      Debug.checkDefined(visitNode(node.name, visitor, isIdentifier)),
    );
  }
  function visitNamedExportBindings(node, allowEmpty) {
    return isNamespaceExport(node)
      ? visitNamespaceExports(node)
      : visitNamedExports(node, allowEmpty);
  }
  function visitExportSpecifier(node) {
    return !node.isTypeOnly &&
        (compilerOptions.verbatimModuleSyntax ||
          resolver.isValueAliasDeclaration(node))
      ? node
      : undefined;
  }
  function shouldEmitImportEqualsDeclaration(node) {
    return shouldEmitAliasDeclaration(node) ||
      !isExternalModule(currentSourceFile) &&
        resolver.isTopLevelValueImportEqualsWithEntityName(node);
  }
  function visitImportEqualsDeclaration(node) {
    if (node.isTypeOnly) {
      return undefined;
    }
    if (isExternalModuleImportEqualsDeclaration(node)) {
      const isReferenced = shouldEmitAliasDeclaration(node);
      return isReferenced ? visitEachChild(node, visitor, context) : undefined;
    }
    if (!shouldEmitImportEqualsDeclaration(node)) {
      return undefined;
    }
    const moduleReference = createExpressionFromEntityName(
      factory,
      node.moduleReference,
    );
    setEmitFlags(
      moduleReference,
      EmitFlags.NoComments | EmitFlags.NoNestedComments,
    );
    if (isNamedExternalModuleExport(node) || !isExportOfNamespace(node)) {
      return setOriginalNode(
        setTextRange(
          factory.createVariableStatement(
            visitNodes(node.modifiers, modifierVisitor, isModifier),
            factory.createVariableDeclarationList([
              setOriginalNode(
                factory.createVariableDeclaration(
                  node.name,
                  undefined,
                  undefined,
                  moduleReference,
                ),
                node,
              ),
            ]),
          ),
          node,
        ),
        node,
      );
    } else {
      return setOriginalNode(
        createNamespaceExport(node.name, moduleReference, node),
        node,
      );
    }
  }
  function isExportOfNamespace(node) {
    return currentNamespace !== undefined &&
      hasSyntacticModifier(node, ModifierFlags.Export);
  }
  function isExternalModuleExport(node) {
    return currentNamespace === undefined &&
      hasSyntacticModifier(node, ModifierFlags.Export);
  }
  function isNamedExternalModuleExport(node) {
    return isExternalModuleExport(node) &&
      !hasSyntacticModifier(node, ModifierFlags.Default);
  }
  function isDefaultExternalModuleExport(node) {
    return isExternalModuleExport(node) &&
      hasSyntacticModifier(node, ModifierFlags.Default);
  }
  function createExportMemberAssignmentStatement(node) {
    const expression = factory.createAssignment(
      factory.getExternalModuleOrNamespaceExportName(
        currentNamespaceContainerName,
        node,
        false,
        true,
      ),
      factory.getLocalName(node),
    );
    setSourceMapRange(
      expression,
      createRange(node.name ? node.name.pos : node.pos, node.end),
    );
    const statement = factory.createExpressionStatement(expression);
    setSourceMapRange(statement, createRange(-1, node.end));
    return statement;
  }
  function addExportMemberAssignment(statements, node) {
    statements.push(createExportMemberAssignmentStatement(node));
  }
  function createNamespaceExport(exportName, exportValue, location) {
    return setTextRange(
      factory.createExpressionStatement(
        factory.createAssignment(
          factory.getNamespaceMemberName(
            currentNamespaceContainerName,
            exportName,
            false,
            true,
          ),
          exportValue,
        ),
      ),
      location,
    );
  }
  function createNamespaceExportExpression(exportName, exportValue, location) {
    return setTextRange(
      factory.createAssignment(
        getNamespaceMemberNameWithSourceMapsAndWithoutComments(exportName),
        exportValue,
      ),
      location,
    );
  }
  function getNamespaceMemberNameWithSourceMapsAndWithoutComments(name) {
    return factory.getNamespaceMemberName(
      currentNamespaceContainerName,
      name,
      false,
      true,
    );
  }
  function getNamespaceParameterName(node) {
    const name = factory.getGeneratedNameForNode(node);
    setSourceMapRange(name, node.name);
    return name;
  }
  function getNamespaceContainerName(node) {
    return factory.getGeneratedNameForNode(node);
  }
  function enableSubstitutionForNonQualifiedEnumMembers() {
    if (
      (enabledSubstitutions &
        TypeScriptSubstitutionFlags.NonQualifiedEnumMembers) === 0
    ) {
      enabledSubstitutions |=
        TypeScriptSubstitutionFlags.NonQualifiedEnumMembers;
      context.enableSubstitution(SyntaxKind.Identifier);
    }
  }
  function enableSubstitutionForNamespaceExports() {
    if (
      (enabledSubstitutions & TypeScriptSubstitutionFlags.NamespaceExports) ===
        0
    ) {
      enabledSubstitutions |= TypeScriptSubstitutionFlags.NamespaceExports;
      context.enableSubstitution(SyntaxKind.Identifier);
      context.enableSubstitution(SyntaxKind.ShorthandPropertyAssignment);
      context.enableEmitNotification(SyntaxKind.ModuleDeclaration);
    }
  }
  function isTransformedModuleDeclaration(node) {
    return getOriginalNode(node).kind === SyntaxKind.ModuleDeclaration;
  }
  function isTransformedEnumDeclaration(node) {
    return getOriginalNode(node).kind === SyntaxKind.EnumDeclaration;
  }
  function onEmitNode(hint, node, emitCallback) {
    const savedApplicableSubstitutions = applicableSubstitutions;
    const savedCurrentSourceFile = currentSourceFile;
    if (isSourceFile(node)) {
      currentSourceFile = node;
    }
    if (
      enabledSubstitutions & TypeScriptSubstitutionFlags.NamespaceExports &&
      isTransformedModuleDeclaration(node)
    ) {
      applicableSubstitutions |= TypeScriptSubstitutionFlags.NamespaceExports;
    }
    if (
      enabledSubstitutions &
        TypeScriptSubstitutionFlags.NonQualifiedEnumMembers &&
      isTransformedEnumDeclaration(node)
    ) {
      applicableSubstitutions |=
        TypeScriptSubstitutionFlags.NonQualifiedEnumMembers;
    }
    previousOnEmitNode(hint, node, emitCallback);
    applicableSubstitutions = savedApplicableSubstitutions;
    currentSourceFile = savedCurrentSourceFile;
  }
  function onSubstituteNode(hint, node) {
    node = previousOnSubstituteNode(hint, node);
    if (hint === EmitHint.Expression) {
      return substituteExpression(node);
    } else if (isShorthandPropertyAssignment(node)) {
      return substituteShorthandPropertyAssignment(node);
    }
    return node;
  }
  function substituteShorthandPropertyAssignment(node) {
    if (enabledSubstitutions & TypeScriptSubstitutionFlags.NamespaceExports) {
      const name = node.name;
      const exportedName = trySubstituteNamespaceExportedName(name);
      if (exportedName) {
        if (node.objectAssignmentInitializer) {
          const initializer = factory.createAssignment(
            exportedName,
            node.objectAssignmentInitializer,
          );
          return setTextRange(
            factory.createPropertyAssignment(name, initializer),
            node,
          );
        }
        return setTextRange(
          factory.createPropertyAssignment(name, exportedName),
          node,
        );
      }
    }
    return node;
  }
  function substituteExpression(node) {
    switch (node.kind) {
      case SyntaxKind.Identifier:
        return substituteExpressionIdentifier(node);
      case SyntaxKind.PropertyAccessExpression:
        return substitutePropertyAccessExpression(node);
      case SyntaxKind.ElementAccessExpression:
        return substituteElementAccessExpression(node);
    }
    return node;
  }
  function substituteExpressionIdentifier(node) {
    return trySubstituteNamespaceExportedName(node) || node;
  }
  function trySubstituteNamespaceExportedName(node) {
    if (
      enabledSubstitutions & applicableSubstitutions &&
      !isGeneratedIdentifier(node) && !isLocalName(node)
    ) {
      const container = resolver.getReferencedExportContainer(node, false);
      if (container && container.kind !== SyntaxKind.SourceFile) {
        const substitute = applicableSubstitutions &
                TypeScriptSubstitutionFlags.NamespaceExports &&
            container.kind === SyntaxKind.ModuleDeclaration ||
          applicableSubstitutions &
                TypeScriptSubstitutionFlags.NonQualifiedEnumMembers &&
            container.kind === SyntaxKind.EnumDeclaration;
        if (substitute) {
          return setTextRange(
            factory.createPropertyAccessExpression(
              factory.getGeneratedNameForNode(container),
              node,
            ),
            node,
          );
        }
      }
    }
    return undefined;
  }
  function substitutePropertyAccessExpression(node) {
    return substituteConstantValue(node);
  }
  function substituteElementAccessExpression(node) {
    return substituteConstantValue(node);
  }
  function safeMultiLineComment(value) {
    return value.replace(/\*\//g, "*_/");
  }
  function substituteConstantValue(node) {
    const constantValue = tryGetConstEnumValue(node);
    if (constantValue !== undefined) {
      setConstantValue(node, constantValue);
      const substitute = typeof constantValue === "string"
        ? factory.createStringLiteral(constantValue)
        : constantValue < 0
        ? factory.createPrefixUnaryExpression(
          SyntaxKind.MinusToken,
          factory.createNumericLiteral(-constantValue),
        )
        : factory.createNumericLiteral(constantValue);
      if (!compilerOptions.removeComments) {
        const originalNode = getOriginalNode(node, isAccessExpression);
        addSyntheticTrailingComment(
          substitute,
          SyntaxKind.MultiLineCommentTrivia,
          ` ${safeMultiLineComment(getTextOfNode(originalNode))} `,
        );
      }
      return substitute;
    }
    return node;
  }
  function tryGetConstEnumValue(node) {
    if (getIsolatedModules(compilerOptions)) {
      return undefined;
    }
    return isPropertyAccessExpression(node) || isElementAccessExpression(node)
      ? resolver.getConstantValue(node)
      : undefined;
  }
  function shouldEmitAliasDeclaration(node) {
    return compilerOptions.verbatimModuleSyntax || isInJSFile(node) ||
      resolver.isReferencedAliasDeclaration(node);
  }
}
