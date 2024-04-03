---
date: 2024-01-20 17:54:10.761489-07:00
description: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u3092\u8AAD\u3080\u3053\
  \u3068\u306F\u3001\u30D5\u30A1\u30A4\u30EB\u306E\u5185\u5BB9\u3092\u30D7\u30ED\u30B0\
  \u30E9\u30E0\u306B\u53D6\u308A\u8FBC\u3080\u3053\u3068\u3067\u3059\u3002\u30C7\u30FC\
  \u30BF\u89E3\u6790\u3001\u8A2D\u5B9A\u306E\u8AAD\u307F\u8FBC\u307F\u3001\u3042\u308B\
  \u3044\u306F\u5358\u306B\u5916\u90E8\u304B\u3089\u306E\u60C5\u5831\u3092\u53CE\u96C6\
  \u3059\u308B\u305F\u3081\u306B\u3001\u30D7\u30ED\u30B0\u30E9\u30DE\u306F\u3053\u308C\
  \u3092\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.581873-06:00'
model: gpt-4-1106-preview
summary: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u3092\u8AAD\u3080\u3053\
  \u3068\u306F\u3001\u30D5\u30A1\u30A4\u30EB\u306E\u5185\u5BB9\u3092\u30D7\u30ED\u30B0\
  \u30E9\u30E0\u306B\u53D6\u308A\u8FBC\u3080\u3053\u3068\u3067\u3059\u3002\u30C7\u30FC\
  \u30BF\u89E3\u6790\u3001\u8A2D\u5B9A\u306E\u8AAD\u307F\u8FBC\u307F\u3001\u3042\u308B\
  \u3044\u306F\u5358\u306B\u5916\u90E8\u304B\u3089\u306E\u60C5\u5831\u3092\u53CE\u96C6\
  \u3059\u308B\u305F\u3081\u306B\u3001\u30D7\u30ED\u30B0\u30E9\u30DE\u306F\u3053\u308C\
  \u3092\u884C\u3044\u307E\u3059\u3002."
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u8AAD\u307F\u8FBC\u307F"
weight: 22
---

## How to: (方法)
```clojure
;; ファイルを読む簡単な例
(with-open [rdr (clojure.java.io/reader "example.txt")]
  (doseq [line (line-seq rdr)]
    (println line)))
```
サンプル出力:
```
これはテキストファイルの最初の行です。
これは二行目です。
```

## Deep Dive (深掘り)
テキストファイルを読むことは、Clojureが登場した2007年から今日まで、ファイルI/Oの基本操作です。 `slurp` 関数でファイルの内容を一度にメモリに読み込み可能ですが、大きなファイルの場合は `line-seq` と `with-open` を使って、必要な行だけを読むことが推奨されます。これによりメモリ使用量が抑えられます。また、`reader` 関数はJavaの `java.io.BufferedReader` に対するラッパーとして機能し、Clojureのコード内でJavaライブラリとスムーズに連携できることを示しています。

## See Also (関連情報)
- Clojureの公式ドキュメンテーション: [https://clojure.org/](https://clojure.org/)
- `java.io`ライブラリとの連携についての詳細: [https://clojure.github.io/clojure/clojure.java.io-api.html](https://clojure.github.io/clojure/clojure.java.io-api.html)
