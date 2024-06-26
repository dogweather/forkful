---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:26.774137-07:00
description: "\u3069\u3046\u3084\u3063\u3066\uFF1A Go\u306B\u306F\u7D44\u307F\u8FBC\
  \u307F\u306EREPL\u304C\u542B\u307E\u308C\u3066\u3044\u307E\u305B\u3093\u304C\u3001\
  \u30B3\u30DF\u30E5\u30CB\u30C6\u30A3\u306F`gore`\u306E\u3088\u3046\u306A\u30C4\u30FC\
  \u30EB\u3092\u4F5C\u6210\u3057\u3066\u305D\u306E\u30AE\u30E3\u30C3\u30D7\u3092\u57CB\
  \u3081\u3066\u3044\u307E\u3059\u3002\u307E\u305A\u3001\u4EE5\u4E0B\u3092\u5B9F\u884C\
  \u3057\u3066`gore`\u3092\u30A4\u30F3\u30B9\u30C8\u30FC\u30EB\u3057\u307E\u3059\uFF1A\
  ."
lastmod: '2024-04-05T22:37:49.718035-06:00'
model: gpt-4-0125-preview
summary: "\u3069\u3046\u3084\u3063\u3066\uFF1A Go\u306B\u306F\u7D44\u307F\u8FBC\u307F\
  \u306EREPL\u304C\u542B\u307E\u308C\u3066\u3044\u307E\u305B\u3093\u304C\u3001\u30B3\
  \u30DF\u30E5\u30CB\u30C6\u30A3\u306F`gore`\u306E\u3088\u3046\u306A\u30C4\u30FC\u30EB\
  \u3092\u4F5C\u6210\u3057\u3066\u305D\u306E\u30AE\u30E3\u30C3\u30D7\u3092\u57CB\u3081\
  \u3066\u3044\u307E\u3059\u3002\u307E\u305A\u3001\u4EE5\u4E0B\u3092\u5B9F\u884C\u3057\
  \u3066`gore`\u3092\u30A4\u30F3\u30B9\u30C8\u30FC\u30EB\u3057\u307E\u3059\uFF1A."
title: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\uFF08REPL\uFF09\
  \u306E\u4F7F\u7528\u65B9\u6CD5"
weight: 34
---

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
