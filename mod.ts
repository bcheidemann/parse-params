import { parse, type Pattern } from "acorn";
import { assert } from "@std/assert";

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
): string[] {
  const fnStr = fn.toString();
  return parseParamNamesFromString(fnStr);
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
export function parseParamNamesFromString(fn: string): string[] {
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
    return mapParams(statement.params);
  }

  assert(
    statement.type === "ExpressionStatement",
    `Expected FunctionDeclaration or ExpressionStatement but received ${statement.type}`,
  );

  assert(
    statement.expression.type === "ArrowFunctionExpression",
    `Expected ArrowFunctionExpression but received ${statement.expression.type}`,
  );

  return mapParams(statement.expression.params);

  function mapParams(params: Pattern[]) {
    return params.map(({ start, end }) => fn.substring(start, end));
  }
}
