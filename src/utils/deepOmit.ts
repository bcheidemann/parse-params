// deno-lint-ignore-file no-explicit-any
export type DeepOmit<TType, TOmitKeys extends string | number | symbol> =
  TType extends Record<any, any> ? DeepOmitObject<TType, TOmitKeys>
    : TType extends any[] ? DeepOmitArray<TType, TOmitKeys>
    : TType;
type DeepOmitObject<
  TObject extends Record<any, any>,
  TOmitKeys extends string | number | symbol,
> = {
  [key in keyof Omit<TObject, TOmitKeys>]: DeepOmit<TObject[key], TOmitKeys>;
};
type DeepOmitArray<
  TArray extends any[],
  TOmitKeys extends string | number | symbol,
> = {
  [key in keyof TArray]: DeepOmit<TArray, TOmitKeys>;
};
