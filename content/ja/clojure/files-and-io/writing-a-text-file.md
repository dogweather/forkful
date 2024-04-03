---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:43.315080-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.582685-06:00'
model: gpt-4-0125-preview
summary: "Clojure\u3067\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u3092\u66F8\
  \u304F\u3053\u3068\u306F\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u5916\
  \u90E8\u306B\u30C7\u30FC\u30BF\u3092\u4FDD\u5B58\u3059\u308B\u305F\u3081\u306E\u30D5\
  \u30A1\u30A4\u30EB\u3092\u4F5C\u6210\u307E\u305F\u306F\u4FEE\u6B63\u3059\u308B\u3053\
  \u3068\u3092\u95A2\u4E0E\u3057\u307E\u3059\u3002\u3053\u308C\u306B\u3088\u308A\u3001\
  \u6C38\u7D9A\u6027\u3001\u8A2D\u5B9A\u3001\u30ED\u30B0\u8A18\u9332\u3001\u307E\u305F\
  \u306F\u30D7\u30ED\u30BB\u30B9\u9593\u901A\u4FE1\u304C\u53EF\u80FD\u306B\u306A\u308A\
  \u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30A2\u30D7\u30EA\
  \u30B1\u30FC\u30B7\u30E7\u30F3\u72B6\u614B\u3001\u8A2D\u5B9A\u3092\u5916\u90E8\u5316\
  \u3059\u308B\u305F\u3081\u3001\u307E\u305F\u306F\u30D7\u30ED\u30B0\u30E9\u30E0\u306E\
  \u7570\u306A\u308B\u90E8\u5206\u3084\u5168\u304F\u7570\u306A\u308B\u30D7\u30ED\u30B0\
  \u30E9\u30E0\u9593\u3067\u60C5\u5831\u3092\u5171\u6709\u3059\u308B\u305F\u3081\u306B\
  \u3001\u3053\u306E\u30BF\u30B9\u30AF\u3092\u5B9F\u884C\u3057\u307E\u3059\u3002."
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
weight: 24
---

## どのように:


### Clojureの組み込み関数を使用してファイルにテキストを書き込む
ファイルにテキストを書き込む最もシンプルな方法は、`spit` 関数を使用することです。これは2つの引数を取ります: ファイルパスと書き込む文字列。ファイルが存在しない場合、`spit`はそれを作成します。存在する場合は、`spit`はそれを上書きします。

```clojure
(spit "example.txt" "Hello, world!")
```

既存のファイルにテキストを追加するには、`spit` 関数に `:append` オプションを使用します。

```clojure
(spit "example.txt" "\nLet's add this new line." :append true)
```

これらのスニペットを実行した後、"example.txt"には以下が含まれます:

```
Hello, world!
Let's add this new line.
```

### サードパーティのライブラリを使用する
Clojureの組み込み機能がしばしば十分な場合がありますが、コミュニティはより複雑または特定のタスクのための堅牢なライブラリを開発しました。ファイルI/Oに関しては、`clojure.java.io`が一般的なライブラリの一つで、よりJavaライクなファイル処理アプローチを提供します。

`clojure.java.io`を使用してファイルに書き込むには、まずそれをインポートする必要があります:

```clojure
(require '[clojure.java.io :as io])
```

その後、`writer` 関数を使用してライターオブジェクトを取得し、`spit` 関数（または`print`, `println`のような他のもの）を使用してファイルに書き込みます:

```clojure
(with-open [w (io/writer "example_with_io.txt")]
  (.write w "This is written using clojure.java.io"))
```

これにより、"example_with_io.txt"が作成されます（既に存在する場合は上書きされます）：

```
This is written using clojure.java.io
```

覚えておくべきは、`with-open`は書き込み後にファイルが適切に閉じられることを保証し、潜在的なリソース漏洩を避けることです。
