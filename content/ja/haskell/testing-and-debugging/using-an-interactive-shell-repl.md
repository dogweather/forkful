---
date: 2024-01-26 04:14:54.902218-07:00
description: "\u4F7F\u3044\u65B9\uFF1A GHCi\uFF08Glasgow Haskell Compiler\u306E\u30A4\
  \u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u74B0\u5883\uFF09\u3092\u8D77\u52D5\u3059\
  \u308B\u306B\u306F\u3001\u7AEF\u672B\u3067\u5358\u306B`ghci`\u3068\u30BF\u30A4\u30D7\
  \u3057\u307E\u3059\u3002\u4F7F\u3044\u65B9\u306F\u4EE5\u4E0B\u306E\u901A\u308A\u3067\
  \u3059\uFF1A."
lastmod: '2024-03-13T22:44:42.186408-06:00'
model: gpt-4-0125-preview
summary: "GHCi\uFF08Glasgow Haskell Compiler\u306E\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\
  \u30A3\u30D6\u74B0\u5883\uFF09\u3092\u8D77\u52D5\u3059\u308B\u306B\u306F\u3001\u7AEF\
  \u672B\u3067\u5358\u306B`ghci`\u3068\u30BF\u30A4\u30D7\u3057\u307E\u3059\u3002\u4F7F\
  \u3044\u65B9\u306F\u4EE5\u4E0B\u306E\u901A\u308A\u3067\u3059\uFF1A."
title: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\uFF08REPL\uFF09\
  \u306E\u4F7F\u7528"
weight: 34
---

## 使い方：
GHCi（Glasgow Haskell Compilerのインタラクティブ環境）を起動するには、端末で単に`ghci`とタイプします。使い方は以下の通りです：

```Haskell
Prelude> let x = 5
Prelude> x * 2
10
Prelude> :t x
x :: Num a => a
```

サンプル出力は、`x`が数値変数であり、それを2倍すると10になることを示しています。

## 深掘り：
HaskellのGHCiは、その開始以来、長い道のりを歩んできました。タブ補完、複数行入力、パッケージの読み込みなど、豊富な機能を提供しています。Hugsのような代替手段は、今ではほとんど歴史的なものであり、GHCiが標準となっています。GHCiは、表現を入力するたびにコードをジャストインタイムでコンパイルし、Haskellコードを効率的にテストする方法を提供します。

## 参照：
- [GHCユーザーガイド – GHCi](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/ghci.html)
- [Learn You a Haskell for Great Good! – Starting Out](http://learnyouahaskell.com/starting-out#hello-world)
- [Haskell Wiki – GHC/GHCi](https://wiki.haskell.org/GHC/GHCi)
