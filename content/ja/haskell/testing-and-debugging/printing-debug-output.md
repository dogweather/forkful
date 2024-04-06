---
date: 2024-01-20 17:52:36.893963-07:00
description: "How to: (\u65B9\u6CD5) Haskell\u3067\u306F\u3001\u30C7\u30D0\u30C3\u30B0\
  \u51FA\u529B\u306F\u526F\u4F5C\u7528\u3092\u4F34\u3046\u305F\u3081\u3001IO\u30E2\
  \u30CA\u30C9\u5185\u3067\u884C\u3044\u307E\u3059\u3002\u904E\u53BB\u306B\u306F\u7D14\
  \u7C8B\u306A\u95A2\u6570\u5185\u3067\u30C7\u30D0\u30C3\u30B0\u3092\u884C\u3046\u3053\
  \u3068\u306F\u63A8\u5968\u3055\u308C\u3066\u3044\u307E\u305B\u3093\u3067\u3057\u305F\
  \u304C\u3001\u4ECA\u65E5\u3067\u306F`Debug.Trace`\u30E2\u30B8\u30E5\u30FC\u30EB\u306E\
  `trace`\u95A2\u6570\u3092\u5229\u7528\u3059\u308B\u3053\u3068\u3082\u53EF\u80FD\u3067\
  \u3059\u3002\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.053302-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) Haskell\u3067\u306F\u3001\u30C7\u30D0\u30C3\u30B0\u51FA\u529B\
  \u306F\u526F\u4F5C\u7528\u3092\u4F34\u3046\u305F\u3081\u3001IO\u30E2\u30CA\u30C9\
  \u5185\u3067\u884C\u3044\u307E\u3059\u3002\u904E\u53BB\u306B\u306F\u7D14\u7C8B\u306A\
  \u95A2\u6570\u5185\u3067\u30C7\u30D0\u30C3\u30B0\u3092\u884C\u3046\u3053\u3068\u306F\
  \u63A8\u5968\u3055\u308C\u3066\u3044\u307E\u305B\u3093\u3067\u3057\u305F\u304C\u3001\
  \u4ECA\u65E5\u3067\u306F`Debug.Trace`\u30E2\u30B8\u30E5\u30FC\u30EB\u306E`trace`\u95A2\
  \u6570\u3092\u5229\u7528\u3059\u308B\u3053\u3068\u3082\u53EF\u80FD\u3067\u3059."
title: "\u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u3092\u8868\u793A\u3059\u308B"
weight: 33
---

## How to: (方法)
```Haskell
-- 'putStrLn' を使って文字列を出力する
main :: IO ()
main = do
  putStrLn "Hello, debug!"

-- 'print' を使って変数の内容を出力する
main :: IO ()
main = do
  let number = 42
  print number
```

例の出力:
```
Hello, debug!
42
```

## Deep Dive (深く潜る)
Haskellでは、デバッグ出力は副作用を伴うため、IOモナド内で行います。過去には純粋な関数内でデバッグを行うことは推奨されていませんでしたが、今日では`Debug.Trace`モジュールの`trace`関数を利用することも可能です。

代替方法として、`printf`関数を使う場合がありますが、これは`Text.Printf`モジュールに含まれています。

```Haskell
import Debug.Trace

-- 'trace' を使って純粋関数でもデバッグ出力をする
myFunc :: Integer -> Integer
myFunc x = trace ("myFunc called with " ++ show x) (x + 1)

main :: IO ()
main = print $ myFunc 10
```

例の出力:
```
myFunc called with 10
11
```

先述の`trace`はデバッグ専用であり、本番環境のコードでは避けるべきです。実装の内部で何が行われていて、どのように評価されているか理解することは重要です。

## See Also (関連情報)
- Haskell の IO モナドについて: https://www.haskell.org/tutorial/io.html
- `Debug.Trace`: http://hackage.haskell.org/package/base-4.14.0.0/docs/Debug-Trace.html
- `printf` 関数: http://hackage.haskell.org/package/base-4.14.0.0/docs/Text-Printf.html
