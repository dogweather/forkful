---
title:                "デバッグ出力を表示する"
aliases:
- /ja/haskell/printing-debug-output.md
date:                  2024-01-20T17:52:36.893963-07:00
model:                 gpt-4-1106-preview
simple_title:         "デバッグ出力を表示する"

tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
デバッグ出力とは、コードが実行中に何が起きているかを知るために、メッセージを表示することです。これによりプログラマーはバグの原因を見つけやすくなります。

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
