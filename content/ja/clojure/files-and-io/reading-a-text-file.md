---
date: 2024-01-20 17:54:10.761489-07:00
description: "How to: (\u65B9\u6CD5) \u30B5\u30F3\u30D7\u30EB\u51FA\u529B."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.527357-06:00'
model: gpt-4-1106-preview
summary: ''
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
