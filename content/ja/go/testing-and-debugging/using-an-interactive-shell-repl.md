---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:26.774137-07:00
description: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\u307E\
  \u305F\u306FRead-Eval-Print Loop\uFF08REPL\uFF09\u3092\u4F7F\u7528\u3059\u308B\u3068\
  \u3001\u30EA\u30A2\u30EB\u30BF\u30A4\u30E0\u3067Go\u30B3\u30FC\u30C9\u3092\u8A66\
  \u884C\u3057\u3001\u30B3\u30DE\u30F3\u30C9\u3092\u5B9F\u884C\u3057\u3066\u5373\u5EA7\
  \u306B\u30D5\u30A3\u30FC\u30C9\u30D0\u30C3\u30AF\u3092\u5F97\u308B\u3053\u3068\u304C\
  \u3067\u304D\u307E\u3059\u3002\u3053\u306E\u30A2\u30D7\u30ED\u30FC\u30C1\u306F\u3001\
  \u5F93\u6765\u306E\u7DE8\u96C6-\u30B3\u30F3\u30D1\u30A4\u30EB-\u2026"
lastmod: '2024-03-13T22:44:41.389106-06:00'
model: gpt-4-0125-preview
summary: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\u307E\
  \u305F\u306FRead-Eval-Print Loop\uFF08REPL\uFF09\u3092\u4F7F\u7528\u3059\u308B\u3068\
  \u3001\u30EA\u30A2\u30EB\u30BF\u30A4\u30E0\u3067Go\u30B3\u30FC\u30C9\u3092\u8A66\
  \u884C\u3057\u3001\u30B3\u30DE\u30F3\u30C9\u3092\u5B9F\u884C\u3057\u3066\u5373\u5EA7\
  \u306B\u30D5\u30A3\u30FC\u30C9\u30D0\u30C3\u30AF\u3092\u5F97\u308B\u3053\u3068\u304C\
  \u3067\u304D\u307E\u3059\u3002\u3053\u306E\u30A2\u30D7\u30ED\u30FC\u30C1\u306F\u3001\
  \u5F93\u6765\u306E\u7DE8\u96C6-\u30B3\u30F3\u30D1\u30A4\u30EB-\u5B9F\u884C\u30B5\
  \u30A4\u30AF\u30EB\u3092\u30D0\u30A4\u30D1\u30B9\u3059\u308B\u305F\u3081\u3001\u5B66\
  \u7FD2\u3001\u30C7\u30D0\u30C3\u30B0\u3001\u30D7\u30ED\u30C8\u30BF\u30A4\u30D4\u30F3\
  \u30B0\u306B\u5E83\u304F\u4F7F\u7528\u3055\u308C\u3066\u304A\u308A\u3001\u958B\u767A\
  \u30D7\u30ED\u30BB\u30B9\u3092\u3088\u308A\u901F\u304F\u3001\u76F4\u611F\u7684\u306B\
  \u3057\u307E\u3059\u3002."
title: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\uFF08REPL\uFF09\
  \u306E\u4F7F\u7528\u65B9\u6CD5"
weight: 34
---

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
