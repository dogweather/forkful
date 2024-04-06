---
date: 2024-01-20 17:54:10.761489-07:00
description: "How to: (\u65B9\u6CD5) \u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\
  \u3092\u8AAD\u3080\u3053\u3068\u306F\u3001Clojure\u304C\u767B\u5834\u3057\u305F\
  2007\u5E74\u304B\u3089\u4ECA\u65E5\u307E\u3067\u3001\u30D5\u30A1\u30A4\u30EBI/O\u306E\
  \u57FA\u672C\u64CD\u4F5C\u3067\u3059\u3002 `slurp` \u95A2\u6570\u3067\u30D5\u30A1\
  \u30A4\u30EB\u306E\u5185\u5BB9\u3092\u4E00\u5EA6\u306B\u30E1\u30E2\u30EA\u306B\u8AAD\
  \u307F\u8FBC\u307F\u53EF\u80FD\u3067\u3059\u304C\u3001\u5927\u304D\u306A\u30D5\u30A1\
  \u30A4\u30EB\u306E\u5834\u5408\u306F `line-seq` \u3068 `with-open`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:50:55.579156-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u3092\u8AAD\
  \u3080\u3053\u3068\u306F\u3001Clojure\u304C\u767B\u5834\u3057\u305F2007\u5E74\u304B\
  \u3089\u4ECA\u65E5\u307E\u3067\u3001\u30D5\u30A1\u30A4\u30EBI/O\u306E\u57FA\u672C\
  \u64CD\u4F5C\u3067\u3059\u3002"
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
