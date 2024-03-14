---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:43.315080-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.582685-06:00'
model: gpt-4-0125-preview
summary: "\u2026"
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
---

{{< edit_this_page >}}

## 何となぜ?

Clojureでテキストファイルを書くことは、アプリケーション外部にデータを保存するためのファイルを作成または修正することを関与します。これにより、永続性、設定、ログ記録、またはプロセス間通信が可能になります。プログラマーは、アプリケーション状態、設定を外部化するため、またはプログラムの異なる部分や全く異なるプログラム間で情報を共有するために、このタスクを実行します。

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
