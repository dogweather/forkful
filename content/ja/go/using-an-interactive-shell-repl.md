---
title:                "インタラクティブシェル（REPL）の使用方法"
aliases:
- ja/go/using-an-interactive-shell-repl.md
date:                  2024-02-03T18:10:26.774137-07:00
model:                 gpt-4-0125-preview
simple_title:         "インタラクティブシェル（REPL）の使用方法"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/using-an-interactive-shell-repl.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

インタラクティブシェルまたはRead-Eval-Print Loop（REPL）を使用すると、リアルタイムでGoコードを試行し、コマンドを実行して即座にフィードバックを得ることができます。このアプローチは、従来の編集-コンパイル-実行サイクルをバイパスするため、学習、デバッグ、プロトタイピングに広く使用されており、開発プロセスをより速く、直感的にします。

## どうやって：

Goには組み込みのREPLが含まれていませんが、コミュニティは`gore`のようなツールを作成してそのギャップを埋めています。まず、以下を実行して`gore`をインストールします：

```
$ go get -u github.com/motemen/gore
```

インストールしたら、ターミナルで`gore`と入力して`gore`を起動します：

```
$ gore
```

Goコマンドを受け付ける準備ができたプロンプトが表示されます。簡単な例を試してみましょう：

```
gore> :import fmt
gore> fmt.Println("Hello, Go REPL!")
```

次のような出力が表示されます：

```
Hello, Go REPL!
```

変数や関数定義は期待通りに機能します。関数を宣言できます：

```
gore> :import math
gore> areaCircle := func(radius float64) float64 {
...> return math.Pi * radius * radius
...> }
gore> fmt.Println("半径4の円の面積：", areaCircle(4))
```

そしてすぐに出力を得ます：

```
半径4の円の面積: 50.26548245743669
```

## 深掘り：

REPLの概念は古く、1960年代のLispマシンにまでさかのぼり、インタラクティブなプログラミング体験を提供しています。PythonやJavaScriptのような言語とは異なり、GoはREPLなしで設計されました。その代わりに、パフォーマンスとシンプルさのためにコンパイルされたバイナリに焦点を当てています。これは、Goのシンプルさの哲学と、スケーラブルで保守可能なソフトウェアのための設計を反映しています。

しかし、`gore`や`goplay`のようなツールは、Goコミュニティのこのギャップを埋めるための資源を示しています。これらのツールはGoコードを動的に解析し、`go/eval`パッケージや類似のメカニズムを使用してリアルタイムで実行しますが、ネイティブのREPL環境と比較していくつかの制限があります。これらの制限は、Goの型システムとコンパイルモデルから生じるもので、その場での評価を困難にします。

REPL環境は教育や迅速なテストに非常に有用ですが、Goエコシステムは通常、ほとんどの開発タスクに対して従来のコンパイルアンドランプロセスを重視しています。Visual Studio CodeやGoLandのようなGoサポート付きのIDEやエディタは、プロフェッショナルな開発にREPLが必要なくなるほど、テストやデバッグのための統合ツールを提供します。

しかし、探索的プログラミング、プロトタイピング、学習のために、`gore`のようなREPLは貴重な代替手段を提供し、他の言語でREPLに慣れているプログラマーがGoでも同様の体験を楽しむことができます。
