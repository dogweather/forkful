---
date: 2024-01-26 04:14:54.902218-07:00
description: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\u3001\
  \u307E\u305F\u306FREPL\uFF08\u8AAD\u307F\u53D6\u308A-\u8A55\u4FA1-\u51FA\u529B\u30EB\
  \u30FC\u30D7\uFF09\u3092Haskell\u3067\u4F7F\u3046\u3068\u3001\u30B3\u30FC\u30C9\u30B9\
  \u30CB\u30DA\u30C3\u30C8\u3092\u30EA\u30A2\u30EB\u30BF\u30A4\u30E0\u3067\u5B9F\u884C\
  \u3067\u304D\u307E\u3059\u3002\u3053\u308C\u306F\u3001\u8FC5\u901F\u306A\u30D5\u30A3\
  \u30FC\u30C9\u30D0\u30C3\u30AF\u3001\u95A2\u6570\u306E\u30C6\u30B9\u30C8\u3001\u305D\
  \u3057\u3066\u8A00\u8A9E\u306E\u5B66\u7FD2\u306E\u305F\u3081\u306E\u904A\u3073\u5834\
  \u3067\u3059\u3002"
lastmod: 2024-02-19 22:05:01.330736
model: gpt-4-0125-preview
summary: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\u3001\
  \u307E\u305F\u306FREPL\uFF08\u8AAD\u307F\u53D6\u308A-\u8A55\u4FA1-\u51FA\u529B\u30EB\
  \u30FC\u30D7\uFF09\u3092Haskell\u3067\u4F7F\u3046\u3068\u3001\u30B3\u30FC\u30C9\u30B9\
  \u30CB\u30DA\u30C3\u30C8\u3092\u30EA\u30A2\u30EB\u30BF\u30A4\u30E0\u3067\u5B9F\u884C\
  \u3067\u304D\u307E\u3059\u3002\u3053\u308C\u306F\u3001\u8FC5\u901F\u306A\u30D5\u30A3\
  \u30FC\u30C9\u30D0\u30C3\u30AF\u3001\u95A2\u6570\u306E\u30C6\u30B9\u30C8\u3001\u305D\
  \u3057\u3066\u8A00\u8A9E\u306E\u5B66\u7FD2\u306E\u305F\u3081\u306E\u904A\u3073\u5834\
  \u3067\u3059\u3002"
title: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\uFF08REPL\uFF09\
  \u306E\u4F7F\u7528"
---

{{< edit_this_page >}}

## 何とその理由？
インタラクティブシェル、またはREPL（読み取り-評価-出力ループ）をHaskellで使うと、コードスニペットをリアルタイムで実行できます。これは、迅速なフィードバック、関数のテスト、そして言語の学習のための遊び場です。

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
