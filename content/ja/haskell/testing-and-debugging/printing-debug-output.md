---
date: 2024-01-20 17:52:36.893963-07:00
description: "How to: (\u65B9\u6CD5) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.187664-06:00'
model: gpt-4-1106-preview
summary: .
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
