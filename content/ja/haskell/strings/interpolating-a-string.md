---
date: 2024-01-20 17:50:59.001766-07:00
description: "\u6587\u5B57\u5217\u88DC\u9593\u306F\u3001\u5909\u6570\u3084\u5F0F\u306E\
  \u5024\u3092\u6587\u5B57\u5217\u4E2D\u306B\u57CB\u3081\u8FBC\u3080\u3053\u3068\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u52D5\u7684\u306B\u5185\u5BB9\u3092\
  \u5909\u66F4\u3059\u308B\u30E1\u30C3\u30BB\u30FC\u30B8\u3084\u30C7\u30FC\u30BF\u3092\
  \u7C21\u5358\u306B\u751F\u6210\u3059\u308B\u305F\u3081\u306B\u4F7F\u3046\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.162718-06:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u88DC\u9593\u306F\u3001\u5909\u6570\u3084\u5F0F\u306E\
  \u5024\u3092\u6587\u5B57\u5217\u4E2D\u306B\u57CB\u3081\u8FBC\u3080\u3053\u3068\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u52D5\u7684\u306B\u5185\u5BB9\u3092\
  \u5909\u66F4\u3059\u308B\u30E1\u30C3\u30BB\u30FC\u30B8\u3084\u30C7\u30FC\u30BF\u3092\
  \u7C21\u5358\u306B\u751F\u6210\u3059\u308B\u305F\u3081\u306B\u4F7F\u3046\u3002"
title: "\u6587\u5B57\u5217\u306E\u88DC\u9593"
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
