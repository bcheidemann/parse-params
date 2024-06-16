# parse-params

[![JSR](https://jsr.io/badges/@bcheidemann/parse-params)](https://jsr.io/@bcheidemann/parse-params)
![publish workflow](https://github.com/bcheidemann/parse-params/actions/workflows/publish.yml/badge.svg)

## Overview

`@bcheidemann/parse-params` is a pure JavaScript runtime function parser, which lists the names of function parameters. It uses [`acorn`](https://github.com/acornjs/acorn) (a "tiny, fast JavaScript parser, written completely in JavaScript") under the hood to parse out the function parameters from stringified function references. It supports anonymous and named functions, async and sync, arrow functions, and class methods. It does not currently support constructors.

## Motivation

Sometimes, admittedly not very often, you might want to know the names of function arguments at runtime. It is used in `@bcheidemann/tracing` to enable the `@instrument(skip('arg'))` syntax for methods.

## Installation

### Deno

```sh
deno add @bcheidemann/parse-params
```

### Node

The package is published to [JSR](https://jsr.io/@bcheidemann/parse-params), a new package registry for TypeScript. To install JSR packages for Node, you need to use the `jsr` CLI. After installing it, it will behave just like any other Node module.

```sh
# npm
npx jsr add @bcheidemann/parse-params
```

```sh
# yarn
yarn dlx jsr add @bcheidemann/parse-params
```

```sh
# pnpm
pnpm dlx jsr add @bcheidemann/parse-params
```

### Bun

```sh
bunx jsr add @bcheidemann/parse-params
```

## Usage

```ts
import { assertEquals } from "@std/assert";
import { parseParamNamesFromFunction } from "@bcheidemann/parse-params";

function example(arg0: string) {}

const params = parseParamNamesFromFunction(example);

assertEquals(params[0], "arg0");
```

## Limitations

Thanks to [`acorn`](https://github.com/acornjs/acorn), `@bcheidemann/parse-params` doesn't have a lot of the same limitations as seen in many of the usual regular expression based implementations. For instance, the arrow function expression `(arg0 = (() => { return { val: 43} })(), arg1) => (arg0.val + arg1.val)` would cause significant issues for regex based implementations, due to the inclusion of arbitrary JavaScript syntax within the function params. This is no problem for `@bcheidemann/parse-params`!

### Performance

In order for `@bcheidemann/parse-params` to offer the level of robustness it does, it ships with a fully featured JavaScript parser ([`acorn`](https://github.com/acornjs/acorn)). Although `acorn` is small and fast by the standards of JavaScript parsers, it does still add some non-trivial overhead. It is therefore recommended to avoid using this package for hot paths in your program. Additionally, it is recommended to try to avoid parsing the same function multiple times.

Note that the entire function, including the function body will be parsed, since we have no way to tell `acorn` to "stop" after the params have been parsed. This can incur additional overhead for large functions.

### Runtime Differences

Different JavaScript runtimes (in particularl those using different engines) are likely to stringify functions slightly differently. This may result in arguments being formatted slightly differently in some cases. This is unlikely to affect simple named arguments, but none the less, it is recommended not to rely on the exact formatting of argument names.

### [native code]

Currently, `@bcheidemann/parse-params` does not support parsing functions with native implementations. For example, in chrome, the `console.log` function is stringified as `function log() { [native code] }`. Since the `[native code]` in the function body is not valid JavaScript, `acorn` cannot make sense of this, and the parsing will fail. We are considering switching the parser from `acorn` to [`acorn-loose`](https://github.com/acornjs/acorn/tree/master/acorn-loose/) (the error-tollerant implementation of `acorn`) but this comes with various tradeoffs, such as a slightly increased bundle size.

### Parameter names

Currently, parameter names are returned in full. For example, `(arg0 = 42) => {}` would return the param name `arg0 = 42`, not `arg0`. This may or may not be desirable depending on your use case, and we may add an option to ignore default values for parameters in future.
