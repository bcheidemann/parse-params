import { parse, type Pattern } from "acorn";
import type { DeepOmit } from "./src/utils/deepOmit.ts";
import { assert, AssertionError } from "./src/utils/assert.ts";

/**
 * Options for `parseParamNamesFromString` and `parseParamNamesFromFunction`.
 */
export type Options = {
  /**
   * If true, the identifier for assignment expressions will be returned, instead of the entire parameter expression.
   *
   * @example With `returnIdentifierForParamAssignmentExpressions` set to `true`:
   * ```ts
   * import { assertEquals } from "@std/assert";
   * import { parseParamNamesFromString } from "@bcheidemann/parse-params";
   *
   * const params = parseParamNamesFromString(
   *   "function example(arg0 = 'default') {}",
   *   { returnIdentifierForParamAssignmentExpressions: true }
   * );
   *
   * assertEquals(params[0], "arg0");
   * ```
   *
   * @example With `returnIdentifierForParamAssignmentExpressions` set to `false`:
   * ```ts
   * import { assertEquals } from "@std/assert";
   * import { parseParamNamesFromString } from "@bcheidemann/parse-params";
   *
   * const params = parseParamNamesFromString(
   *   "function example(arg0 = 'default') {}",
   *   { returnIdentifierForParamAssignmentExpressions: false },
   * );
   *
   * assertEquals(params[0], "arg0 = 'default'");
   * ```
   *
   * @default false
   */
  returnIdentifierForParamAssignmentExpressions?: boolean;
};

/**
 * The parameter node
 */
export type ParamNode = DeepOmit<Pattern, "start" | "end" | "range" | "loc"> & {
  /**
   * The raw text of the parameter.
   */
  rawText: string;
};

/**
 * Parses the parameters for the provided function reference.
 *
 * @example
 * ```ts
 * import { assertEquals } from "@std/assert";
 * import { parseParamNamesFromFunction } from "@bcheidemann/parse-params";
 *
 * function example(arg0: string) {}
 *
 * const params = parseParamNamesFromFunction(example);
 *
 * assertEquals(params[0], "arg0");
 * ```
 *
 * @param fn The function for which to parse the parameters
 * @returns Array of stringified parameters
 */
export function parseParamNamesFromFunction(
  // deno-lint-ignore no-explicit-any
  fn: (...args: any[]) => any,
  options?: Options,
): string[] {
  const fnStr = fn.toString();
  return parseParamNamesFromString(fnStr, options);
}

/**
 * Parses the parameter AST nodes for the provided function reference.
 *
 * @example
 * ```ts
 * import { assertEquals } from "@std/assert";
 * import { parseParamNodesFromFunction } from "@bcheidemann/parse-params";
 *
 * function example(arg0: string) {}
 *
 * const params = parseParamNodesFromFunction(example);
 *
 * assertEquals(params[0], { type: "Identifier", name: "arg0" });
 * ```
 *
 * @param fn The function for which to parse the parameters
 * @returns Array of parameter AST nodes
 */
export function parseParamNodesFromFunction(
  // deno-lint-ignore no-explicit-any
  fn: (...args: any[]) => any,
): ParamNode[] {
  const fnStr = fn.toString();
  return parseParamNodesFromString(fnStr);
}

const ANONYMOUS_SYNC_FUNCTION_REGEX = /^function\s*\(/g;
const ANONYMOUS_ASYNC_FUNCTION_REGEX = /^async function\s*\(/g;
const SYNC_METHOD_REGEX = /^[\w$]+\(/g;
const ASYNC_METHOD_REGEX = /^async [\w$]+\(/g;

/**
 * Parses the parameters for the provided stringified function.
 *
 * @example
 * ```ts
 * import { assertEquals } from "@std/assert";
 * import { parseParamNamesFromString } from "@bcheidemann/parse-params";
 *
 * function example(arg0: string) {}
 *
 * const params = parseParamNamesFromString(example.toString());
 *
 * assertEquals(params[0], "arg0");
 * ```
 *
 * @param fn The stringified function for which to parse the parameters
 * @returns Array of stringified parameters
 */
export function parseParamNamesFromString(
  fn: string,
  options?: Options,
): string[] {
  const params = parseParamNodesFromString(fn);

  return params.map((param) => paramNodeToParamName(param, options));
}

/**
 * Converts a function parameter node into it's name.
 *
 * @param param The parameter node
 * @param options
 * @returns The parameter name
 */
export function paramNodeToParamName(
  param: ParamNode,
  options?: Options,
): string {
  switch (param.type) {
    case "AssignmentPattern":
      if (
        options?.returnIdentifierForParamAssignmentExpressions &&
        param.left.type === "Identifier"
      ) {
        return param.left.name;
      }
      return param.rawText;
    case "ArrayPattern":
    case "MemberExpression":
    case "ObjectPattern":
    case "RestElement":
      return param.rawText;
    case "Identifier":
      return param.name;
    // deno-lint-ignore no-case-declarations
    default:
      const _: never = param;
      // deno-lint-ignore no-explicit-any
      throw new AssertionError("Unexpected param type: " + (param as any).type);
  }
}

/**
 * Parses the parameter AST nodes for the provided stringified function.
 *
 * @example
 * ```ts
 * import { assertEquals } from "@std/assert";
 * import { parseParamNodesFromString } from "@bcheidemann/parse-params";
 *
 * function example(arg0: string) {}
 *
 * const params = parseParamNodesFromString(example.toString());
 *
 * assertEquals(params[0], { type: "Identifier", name: "arg0" });
 * ```
 *
 * @param fn The stringified function for which to parse the parameters
 * @returns Array of parameter AST nodes
 */
export function parseParamNodesFromString(
  fn: string,
): ParamNode[] {
  if (ANONYMOUS_SYNC_FUNCTION_REGEX.test(fn)) {
    fn = fn.replace(ANONYMOUS_SYNC_FUNCTION_REGEX, "function fn(");
  } else if (ANONYMOUS_ASYNC_FUNCTION_REGEX.test(fn)) {
    fn = fn.replace(ANONYMOUS_ASYNC_FUNCTION_REGEX, "async function fn(");
  } else if (SYNC_METHOD_REGEX.test(fn)) {
    fn = fn.replace(SYNC_METHOD_REGEX, "function fn(");
  } else if (ASYNC_METHOD_REGEX.test(fn)) {
    fn = fn.replace(ASYNC_METHOD_REGEX, "async function fn(");
  }

  const program = parse(fn, {
    ecmaVersion: "latest",
  });

  assert(program.body.length === 1);

  const statement = program.body[0];

  if (statement.type === "FunctionDeclaration") {
    return mapPatternsToParamNodes(fn, statement.params);
  }

  assert(
    statement.type === "ExpressionStatement",
    `Expected FunctionDeclaration or ExpressionStatement but received ${statement.type}`,
  );

  assert(
    statement.expression.type === "ArrowFunctionExpression",
    `Expected ArrowFunctionExpression but received ${statement.expression.type}`,
  );

  return mapPatternsToParamNodes(fn, statement.expression.params);
}

function mapPatternsToParamNodes(fn: string, params: Pattern[]): ParamNode[] {
  return params.map((param) => {
    const rawText = fn.substring(param.start, param.end);
    return {
      ...param,
      rawText,
    };
  });
}
