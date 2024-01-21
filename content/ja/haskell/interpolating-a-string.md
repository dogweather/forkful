---
title:                "文字列の補間"
date:                  2024-01-20T17:50:59.001766-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の補間"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列補間は、変数や式の値を文字列中に埋め込むこと。プログラマーは、動的に内容を変更するメッセージやデータを簡単に生成するために使う。

## How to: (方法)
```Haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Data.Text (Text)
import NeatInterpolation (text)

name = "世界"
greeting :: Text
greeting = [text|こんにちは、${name}さん!|]

main :: IO ()
main = putStrLn $ toString greeting
-- 出力: こんにちは、世界さん!
```

## Deep Dive (深掘り)
Haskellでは文字列補間は言語の基本機能ではない。しかし、QuasiQuotes構文を使用するライブラリ、例えば`NeatInterpolation`が使い道を拡げる。歴史的には、他の言語の影響を受けている。代替手段としては、`printf`や`++`を使うことがあるが、QuasiQuotesを使った方法はより直感的で読みやすい。Haskellの型の安全性に影響を与えずに、動的な文字列を扱えるのが魅力。

## See Also (関連情報)
- `NeatInterpolation`ライブラリ: [Hackage NeatInterpolation](https://hackage.haskell.org/package/neat-interpolation)
- Haskellの `QuasiQuotes`: [Haskell Wiki QuasiQuotes](https://wiki.haskell.org/Quasiquotation)
- 文字列操作の基本: [Haskell Wiki Strings](https://wiki.haskell.org/Strings)