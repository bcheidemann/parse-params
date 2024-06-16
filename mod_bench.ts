import { assert } from "@std/assert";
import { transformTypeScript } from "./data/mod_bench.js";
import { parseParamNamesFromFunction } from "./mod.ts";

Deno.bench("parseParamNamesFromFunction", () => {
  const params = parseParamNamesFromFunction(transformTypeScript);
  assert(params.length === 1);
});
