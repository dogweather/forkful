---
date: 2024-01-20 17:50:59.001766-07:00
description: null
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.033632-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) Haskell\u3067\u306F\u6587\u5B57\u5217\u88DC\u9593\u306F\u8A00\
  \u8A9E\u306E\u57FA\u672C\u6A5F\u80FD\u3067\u306F\u306A\u3044\u3002\u3057\u304B\u3057\
  \u3001QuasiQuotes\u69CB\u6587\u3092\u4F7F\u7528\u3059\u308B\u30E9\u30A4\u30D6\u30E9\
  \u30EA\u3001\u4F8B\u3048\u3070`NeatInterpolation`\u304C\u4F7F\u3044\u9053\u3092\u62E1\
  \u3052\u308B\u3002\u6B74\u53F2\u7684\u306B\u306F\u3001\u4ED6\u306E\u8A00\u8A9E\u306E\
  \u5F71\u97FF\u3092\u53D7\u3051\u3066\u3044\u308B\u3002\u4EE3\u66FF\u624B\u6BB5\u3068\
  \u3057\u3066\u306F\u3001`printf`\u3084`++`\u3092\u4F7F\u3046\u3053\u3068\u304C\u3042\
  \u308B\u304C\u3001QuasiQuotes\u3092\u4F7F\u3063\u305F\u65B9\u6CD5\u306F\u3088\u308A\
  \u76F4\u611F\u7684\u3067\u8AAD\u307F\u3084\u3059\u3044\u3002Haskell\u306E\u578B\u306E\
  \u5B89\u5168\u6027\u306B\u5F71\u97FF\u3092\u4E0E\u3048\u305A\u306B\u3001\u52D5\u7684\
  \u306A\u6587\u5B57\u5217\u3092\u6271\u3048\u308B\u306E\u304C\u9B45\u529B\u3002"
title: "\u6587\u5B57\u5217\u306E\u88DC\u9593"
weight: 8
---

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
