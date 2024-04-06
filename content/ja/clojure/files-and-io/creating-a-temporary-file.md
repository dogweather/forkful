---
date: 2024-01-20 17:39:57.424166-07:00
description: "How to: (\u3084\u308A\u65B9) \u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u306E\
  \u6982\u5FF5\u306F\u3001\u30B7\u30B9\u30C6\u30E0\u304C\u4E0D\u5B89\u5B9A\u306B\u306A\
  \u308A\u304C\u3061\u306A\u521D\u671F\u306E\u30B3\u30F3\u30D4\u30E5\u30FC\u30C6\u30A3\
  \u30F3\u30B0\u6642\u4EE3\u306B\u8D77\u6E90\u3092\u6301\u3061\u307E\u3059\u3002\u30C7\
  \u30FC\u30BF\u4FDD\u8B77\u3068\u30B7\u30B9\u30C6\u30E0\u306E\u5B89\u5B9A\u6027\u3092\
  \u9AD8\u3081\u308B\u305F\u3081\u306B\u5C0E\u5165\u3055\u308C\u307E\u3057\u305F\u3002\
  \ Clojure\u3067\u306F`clojure.java.io`\u30E9\u30A4\u30D6\u30E9\u30EA\u306E`create-temp-\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.529680-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) \u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u306E\u6982\u5FF5\
  \u306F\u3001\u30B7\u30B9\u30C6\u30E0\u304C\u4E0D\u5B89\u5B9A\u306B\u306A\u308A\u304C\
  \u3061\u306A\u521D\u671F\u306E\u30B3\u30F3\u30D4\u30E5\u30FC\u30C6\u30A3\u30F3\u30B0\
  \u6642\u4EE3\u306B\u8D77\u6E90\u3092\u6301\u3061\u307E\u3059\u3002\u30C7\u30FC\u30BF\
  \u4FDD\u8B77\u3068\u30B7\u30B9\u30C6\u30E0\u306E\u5B89\u5B9A\u6027\u3092\u9AD8\u3081\
  \u308B\u305F\u3081\u306B\u5C0E\u5165\u3055\u308C\u307E\u3057\u305F."
title: "\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
weight: 21
---

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
