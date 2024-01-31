---
title:                "ディレクトリが存在するかどうかを確認する"
date:                  2024-01-19
html_title:           "Bash: ディレクトリが存在するかどうかを確認する"
simple_title:         "ディレクトリが存在するかどうかを確認する"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
ディレクトリの存在確認は、指定したパスにディレクトリが存在するかをチェックすることです。プログラマはこの確認を行い、ファイル操作や設定読み込み時のエラーを予防します。

## How to: (方法)
Clojureでは`java.io.File`を使ってディレクトリが存在するかをチェックします。簡単な例を見てみましょう。

```Clojure
(require '[clojure.java.io :as io])

(defn directory-exists? [path]
  (let [dir (io/file path)]
    (.exists dir)))

;; 使い方の例：
(println (directory-exists? "/path/to/directory")) ; 存在する場合は true, そうでない場合は false
```

サンプル出力はこのようになります：

```Clojure
true ; もしくは false
```

## Deep Dive (深掘り)
過去、Clojureプログラマーは `java.io.File` の `.exists` メソッドを使ってディレクトリがあるかどうかを確かめてきました。Javaのライブラリを利用するのはClojureの一般的な方法です。代替手段として、`clojure.java.io/file`関数を使用する方法もありますが、これも内部的にはJavaのクラスを使っています。

ディレクトリかどうかをチェックする場合、`.isDirectory`メソッドも組み合わせて使われることがあります。これにより、指定されたパスが実際にディレクトリであるかを確認することができます。

## See Also (関連情報)
- Clojureの公式ドキュメント: https://clojure.org/
- `java.io.File`ドキュメント: https://docs.oracle.com/javase/7/docs/api/java/io/File.html
- Clojureの入門書: "Clojure for the Brave and True" (英語): https://www.braveclojure.com/clojure-for-the-brave-and-true/
