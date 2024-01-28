---
title:                "インタラクティブシェル（REPL）の使用"
date:                  2024-01-26T04:14:29.732882-07:00
model:                 gpt-4-0125-preview
simple_title:         "インタラクティブシェル（REPL）の使用"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## 何となぜ？

REPL（Read-Eval-Print Loopの略）は、コードを対話的に実行し、結果を即座に確認できるプログラミングツールです。プログラマーは、新しい言語を学んだり、デバッグしたり、Gleamのようにその場で実験するためにこれを使用します。

## 方法：

現在、Gleamには標準配布版の中にREPLが含まれていません。しかし、GleamはErlangのバイトコードにコンパイルされるため、既存のErlangシェルを使用してGleamコードを実験することができます。以下の方法で行います：

1. GleamコードをErlangにコンパイルします。
```プレーンテキスト
gleam build
```

2. Erlangシェルを起動します。
```プレーンテキスト
erl -pa ebin
```

3. Gleam関数を呼び出します（`my_mod`という名前のモジュールと`my_fun`という関数を持っていると仮定します）。
```erlang
my_mod:my_fun().
```

シェルに関数の出力が表示されるはずです。

## 深掘り

REPLは、1960年代のLISPのREPLに遡る、多くの関数型プログラミング言語の動的で探索的な精神を体現しています。比較的、Pythonの`ipython`やRubyの`irb`などの他のシステムは、それぞれのコミュニティに同様の体験を提供します。

GleamにはまだネイティブREPLがありませんが、Erlangシェルを利用することは賢い回避策です。Erlangシェルの機能は、Elixir、LFE、Gleamを含むErlangエコシステムを支えるBEAM VM（仮想マシン）から来ています。

GleamエコシステムでのREPLの代替品には、テストケースの作成や、Gleamをサポートしているオンラインコンパイラやコードプレイグラウンドを使用して、完全なプロジェクト設定の外でコードスニペットをテストすることが含まれます。

専用のGleam REPLの実装は、主にGleamとErlangのランタイムのコンパイルされた性質、およびホットコードスワッピングが標準である環境において、課題に直面しています。将来のGleam REPLは、言語の静的型付けとREPLが期待する動的実行環境を調和させる必要があります。

## 参照

- Gleamの公式ドキュメント: https://gleam.run/book/
- Erlangのシェルドキュメント: http://erlang.org/doc/man/erl.html
- Gleamコンパイラのオンラインプレイグラウンド: https://gleam.run/compiler/
