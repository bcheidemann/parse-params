import { describe, it } from "@std/testing/bdd";
import { assertEquals } from "@std/assert";
import {
  parseParamNamesFromFunction,
  parseParamNamesFromString,
} from "./mod.ts";

describe("parseParamNamesFromString", () => {
  it("should return no param names if the function has no params", () => {
    // Arrange
    const fn = "function fn() {}";

    // Act
    const params = parseParamNamesFromString(fn);

    // Assert
    assertEquals(params.length, 0);
  });

  it("should handle function names with _, $, and number characters", () => {
    // Arrange
    const fn = "function $_fn1() {}";

    // Act
    const params = parseParamNamesFromString(fn);

    // Assert
    assertEquals(params.length, 0);
  });

  it("should handle rest parameters", () => {
    // Arrange
    const fn = "function fn(...args) {}";

    // Act
    const params = parseParamNamesFromString(fn);

    // Assert
    assertEquals(params[0], "...args");
  });

  it("should handle default parameters", () => {
    // Arrange
    const fn = "function fn(a = 1) {}";

    // Act
    const params = parseParamNamesFromString(fn);

    // Assert
    assertEquals(params[0], "a = 1");
  });

  it("should handle /* comments */", () => {
    // Arrange
    const fn = "function fn(/* comment */ a) {}";

    // Act
    const params = parseParamNamesFromString(fn);

    // Assert
    assertEquals(params[0], "a");
  });

  it("should handle // comments", () => {
    // Arrange
    const fn = "function fn(// comment\na) {}";

    // Act
    const params = parseParamNamesFromString(fn);

    // Assert
    assertEquals(params[0], "a");
  });

  it("should handle array destructuring", () => {
    // Arrange
    const fn = "function fn([a, b]) {}";

    // Act
    const params = parseParamNamesFromString(fn);

    // Assert
    assertEquals(params[0], "[a, b]");
  });

  it("shoulld handle nested array destructuring", () => {
    // Arrange
    const fn = "function fn([a, [b, c]]) {}";

    // Act
    const params = parseParamNamesFromString(fn);

    // Assert
    assertEquals(params[0], "[a, [b, c]]");
  });

  it("handle array destructuring with rest params", () => {
    // Arrange
    const fn = "function fn([a, ...b]) {}";

    // Act
    const params = parseParamNamesFromString(fn);

    // Assert
    assertEquals(params[0], "[a, ...b]");
  });

  it("should handle array destructuring with rest and default params", () => {
    // Arrange
    const fn = "function fn([a, b = 42, ...c]) {}";

    // Act
    const params = parseParamNamesFromString(fn);

    // Assert
    assertEquals(params[0], "[a, b = 42, ...c]");
  });

  it("should handle array destructuring with rest params, default params and nested object destructuring", () => {
    // Arrange
    const fn = "function fn([a, b = 42, { c }, ...d]) {}";

    // Act
    const params = parseParamNamesFromString(fn);

    // Assert
    assertEquals(params[0], "[a, b = 42, { c }, ...d]");
  });

  it("should handle array destructuring with rest params, default params and nested array destructuring", () => {
    // Arrange
    const fn = "function fn([a, b = 42, [ c ], ...d]) {}";

    // Act
    const params = parseParamNamesFromString(fn);

    // Assert
    assertEquals(params[0], "[a, b = 42, [ c ], ...d]");
  });

  it("should handle array destructuring with nested /* comments */", () => {
    // Arrange
    const fn = "function fn([a, /* comment */ b]) {}";

    // Act
    const params = parseParamNamesFromString(fn);

    // Assert
    assertEquals(params[0], "[a, /* comment */ b]");
  });

  it("should handle object destructuring", () => {
    // Arrange
    const fn = "function fn({ a, b }) {}";

    // Act
    const params = parseParamNamesFromString(fn);

    // Assert
    assertEquals(params[0], "{ a, b }");
  });

  it("should handle object destructuring with rest params", () => {
    // Arrange
    const fn = "function fn({ a, ...b }) {}";

    // Act
    const params = parseParamNamesFromString(fn);

    // Assert
    assertEquals(params[0], "{ a, ...b }");
  });

  it("should handle object destructuring with rest and default params", () => {
    // Arrange
    const fn = "function fn({ a, b = 42, ...c }) {}";

    // Act
    const params = parseParamNamesFromString(fn);

    // Assert
    assertEquals(params[0], "{ a, b = 42, ...c }");
  });

  it("should handle object destructuring with nested /* comments */", () => {
    // Arrange
    const fn = "function fn({ a, /* comment */ b }) {}";

    // Act
    const params = parseParamNamesFromString(fn);

    // Assert
    assertEquals(params[0], "{ a, /* comment */ b }");
  });

  it("should handle object destructuring with nested // comments", () => {
    // Arrange
    const fn = "function fn({ a, // comment\n b }) {}";

    // Act
    const params = parseParamNamesFromString(fn);

    // Assert
    assertEquals(params[0], "{ a, // comment\n b }");
  });

  it("should parse a complex function", () => {
    // Arrange
    const fn =
      `function $_fn(a = { a: 1, b: 2 }, [a1, b1, ...c1], // asdasd }} ) ] ] 
                              [a2], { a3, /* ]}) */ b3 = { c3: 23 } }, ...b) {}`;

    // Act
    const params = parseParamNamesFromString(fn);

    // Assert
    assertEquals(params[0], "a = { a: 1, b: 2 }");
    assertEquals(params[1], "[a1, b1, ...c1]");
    assertEquals(params[2], "[a2]");
    assertEquals(params[3], "{ a3, /* ]}) */ b3 = { c3: 23 } }");
    assertEquals(params[4], "...b");
  });
});

describe("parseParamNamesFromFunction", () => {
  it("should handle the readme example", () => {
    // Arrange
    const fn = (
      arg0 = (() => {
        return { val: 43 };
      })(),
      arg1: { val: number },
    ) => (arg0.val + arg1.val);

    // Act
    const params = parseParamNamesFromFunction(fn);

    // Assert
    assertEquals(
      params[0],
      `arg0 = (()=>{
      return {
        val: 43
      };
    })()`,
    );
    assertEquals(params[1], "arg1");
  });

  it("should handle anonymous sync functions", () => {
    // Arrange
    const fn = function (_arg0: unknown) {};

    // Act
    const params = parseParamNamesFromFunction(fn);

    // Assert
    assertEquals(params[0], "_arg0");
  });

  it("should handle named sync functions", () => {
    // Arrange
    function fn(_arg0: unknown) {}

    // Act
    const params = parseParamNamesFromFunction(fn);

    // Assert
    assertEquals(params[0], "_arg0");
  });

  it("should handle anonymous async functions", () => {
    // Arrange
    const fn = async function (_arg0: unknown) {};

    // Act
    const params = parseParamNamesFromFunction(fn);

    // Assert
    assertEquals(params[0], "_arg0");
  });

  it("should handle named async functions", () => {
    // Arrange
    async function fn(_arg0: unknown) {}

    // Act
    const params = parseParamNamesFromFunction(fn);

    // Assert
    assertEquals(params[0], "_arg0");
  });

  it("should handle sync arrow functions", () => {
    // Arrange
    const fn = (_arg0: unknown) => {};

    // Act
    const params = parseParamNamesFromFunction(fn);

    // Assert
    assertEquals(params[0], "_arg0");
  });

  it("should handle async arrow functions", () => {
    // Arrange
    const fn = async (_arg0: unknown) => {};

    // Act
    const params = parseParamNamesFromFunction(fn);

    // Assert
    assertEquals(params[0], "_arg0");
  });

  it("should handle sync methods", () => {
    // Arrange
    class Test {
      method(_arg0: unknown) {}
    }
    const instance = new Test();

    // Act
    const params = parseParamNamesFromFunction(instance.method);

    // Assert
    assertEquals(params[0], "_arg0");
  });

  it("should handle async methods", () => {
    // Arrange
    class Test {
      async method(_arg0: unknown) {}
    }
    const instance = new Test();

    // Act
    const params = parseParamNamesFromFunction(instance.method);

    // Assert
    assertEquals(params[0], "_arg0");
  });

  it("should respect the returnIdentifierForParamAssignmentExpressions option", () => {
    // Arrange
    const fn = function (_arg0 = 42) {};

    // Act
    const params = parseParamNamesFromFunction(fn, {
      returnIdentifierForParamAssignmentExpressions: true,
    });

    // Assert
    assertEquals(params[0], "_arg0");
  });
});
