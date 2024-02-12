---
title:                "インタラクティブシェル（REPL）の使用"
aliases:
- /ja/haskell/using-an-interactive-shell-repl/
date:                  2024-01-26T04:14:54.902218-07:00
model:                 gpt-4-0125-preview
simple_title:         "インタラクティブシェル（REPL）の使用"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/using-an-interactive-shell-repl.md"
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
