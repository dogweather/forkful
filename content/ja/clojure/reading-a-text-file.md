---
title:                "テキストファイルの読み込み"
date:                  2024-01-20T17:54:10.761489-07:00
model:                 gpt-4-1106-preview
simple_title:         "テキストファイルの読み込み"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (なにを? そしてなぜ?)
テキストファイルを読むことは、ファイルの内容をプログラムに取り込むことです。データ解析、設定の読み込み、あるいは単に外部からの情報を収集するために、プログラマはこれを行います。

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