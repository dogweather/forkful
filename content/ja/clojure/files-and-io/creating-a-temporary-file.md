---
date: 2024-01-20 17:39:57.424166-07:00
description: "\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u306F\u30C7\u30FC\u30BF\u3092\u4E00\
  \u6642\u7684\u306B\u4FDD\u5B58\u3059\u308B\u305F\u3081\u306B\u4F5C\u6210\u3055\u308C\
  \u308B\u30D5\u30A1\u30A4\u30EB\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u30C7\u30FC\u30BF\u306E\u51E6\u7406\u4E2D\u306B\u30C7\u30A3\u30B9\u30AF\u4E0A\
  \u3067\u5B89\u5168\u306B\u64CD\u4F5C\u3057\u305F\u3044\u6642\u3084\u3001\u5927\u91CF\
  \u306E\u30C7\u30FC\u30BF\u3092\u4E00\u6642\u7684\u306B\u6271\u3046\u305F\u3081\u306B\
  \u3053\u308C\u3092\u4F7F\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.583838-06:00'
model: gpt-4-1106-preview
summary: "\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u306F\u30C7\u30FC\u30BF\u3092\u4E00\
  \u6642\u7684\u306B\u4FDD\u5B58\u3059\u308B\u305F\u3081\u306B\u4F5C\u6210\u3055\u308C\
  \u308B\u30D5\u30A1\u30A4\u30EB\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u30C7\u30FC\u30BF\u306E\u51E6\u7406\u4E2D\u306B\u30C7\u30A3\u30B9\u30AF\u4E0A\
  \u3067\u5B89\u5168\u306B\u64CD\u4F5C\u3057\u305F\u3044\u6642\u3084\u3001\u5927\u91CF\
  \u306E\u30C7\u30FC\u30BF\u3092\u4E00\u6642\u7684\u306B\u6271\u3046\u305F\u3081\u306B\
  \u3053\u308C\u3092\u4F7F\u3044\u307E\u3059\u3002."
title: "\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
weight: 21
---

## What & Why? (何となぜ？)

一時ファイルはデータを一時的に保存するために作成されるファイルです。プログラマーはデータの処理中にディスク上で安全に操作したい時や、大量のデータを一時的に扱うためにこれを使います。

## How to: (やり方)

```Clojure
(require '[clojure.java.io :as io])

;; 一時ファイルを作成する
(with-open [temp-file (io/file (io/create-temp-file "prefix-" ".suffix"))]
  (spit temp-file "一時的なコンテンツです"))
;; => temp-fileには作成されたファイルのパスが含まれています。

;; 一時ファイルの内容を読み込む
(slurp temp-file)
;; => "一時的なコンテンツです"
```

## Deep Dive (深く掘り下げて)

一時ファイルの概念は、システムが不安定になりがちな初期のコンピューティング時代に起源を持ちます。データ保護とシステムの安定性を高めるために導入されました。

Clojureでは`clojure.java.io`ライブラリの`create-temp-file`関数を使って一時ファイルを簡単に作成できます。これはJavaの`File.createTempFile`メソッドをラップしているため、Javaプラットフォームでの実績ある方法です。

代替方法として、特定のディレクトリに自分で一時ファイルを管理することもできますが、`create-temp-file`は名前の衝突を避け、システムのテンポラリフォルダを自動で使うため、通常のベストプラクティスです。

## See Also (関連情報)

- [Clojure Docs - clojure.java.io library](https://clojuredocs.org/clojure.java.io)
- [Java Platform SE - Class File](https://docs.oracle.com/javase/7/docs/api/java/io/File.html#createTempFile(java.lang.String,%20java.lang.String))
- [Clojure from the ground up - I/O](https://aphyr.com/posts/309-clojure-from-the-ground-up-io)
