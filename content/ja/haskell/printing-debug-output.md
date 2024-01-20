---
title:                "デバッグ出力の印刷"
html_title:           "Fish Shell: デバッグ出力の印刷"
simple_title:         "デバッグ出力の印刷"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## 何と何のために? (What & Why?)

デバッグ出力の印刷は、コードがどのように動作し、何が起こっているのかを調べるためのプログラマーの手段です。これにより、エラーの発見と修正が容易になります。

## どうやって (How to)

Haskellでは、プリントデバッグは `Debug.Trace` モジュールを使って行います。以下に簡単な例を示します:

```Haskell
import Debug.Trace

main = putStrLn (trace "This will be printed to the console" "Hello, world!")
```

これを実行すると、"This will be printed to the console" と "Hello, world!" の両方が出力されます。

## ディープダイブ (Deep Dive)

`Debug.Trace` モジュールは、Haskellを純粋関数型言語として保持しながら標準出力に文字列を出力する機能を提供します。これは歴史的には、IO処理と辺り合いで特殊な例外を含むHaskellの純粋性と相互作用する方法として追加されました。

それでは、なぜ通常の`putStrLn`を使わないのでしょうか？その理由は、`trace`が純粋なコードの中で使用でき、IOアクションを扱うのに便利だからです。

ただし、`Debug.Trace`の関数はIOをバイパスするため、副作用が発生します。そのため、最終的なプログラムでは使用すべきではありません。デバッグ目的でのみ使用することをお勧めします。

## 参考文献 (See Also)

より詳しい情報や別の視点を得るためには、以下のページが役立つかもしれません。

- [Debug.Traceの公式ドキュメント](https://hackage.haskell.org/package/base-4.14.1.0/docs/Debug-Trace.html)
- [HaskellのIO](http://learnyouahaskell.com/input-and-output)についての詳細なガイド