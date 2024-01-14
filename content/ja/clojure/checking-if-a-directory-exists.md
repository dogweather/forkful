---
title:                "Clojure: ディレクトリが存在するかどうかをチェックする"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜ

ディレクトリの存在をチェックすることの重要性は、コンピュータプログラムにおいて必須です。例えば、あなたのアプリケーションが特定のディレクトリ内にファイルを作成する必要がある時、そのディレクトリが存在しない場合、アプリケーションが正しく機能しない可能性があります。そのため、ディレクトリの存在を確認することは、プログラムの信頼性を確保するために重要です。

## 方法

Clojureでは、ファイルシステムを操作するためのたくさんの便利な関数が用意されています。その中の一つが、`clojure.java.io/file`関数です。この関数を使用することで、ファイルやディレクトリのパスを指定してJavaの`java.io.File`オブジェクトを作成できます。そして、`.exists`メソッドを使用して、ファイルやディレクトリが実際に存在するかどうかを確認できます。

```Clojure
(require '[clojure.java.io :as io])

;; パスを指定してFileオブジェクトを作成
(def dir-path (io/file "path/to/directory"))

;; ディレクトリが存在するかどうかを確認
(.exists dir-path)
```

確認したいパスを指定して`io/file`関数を使い、その返り値として得られる`File`オブジェクトに対して、Javaのメソッドを使用することで簡単にディレクトリの存在をチェックすることができます。

## 深堀り

Clojureの`clojure.java.io`ネームスペースには、ディレクトリの存在を確認する方法以外にも、ファイルやディレクトリを作成したり削除したりするための関数が豊富に用意されています。また、`clojure.java.io.file`関数を使うことで、ディレクトリだけでなく、ファイルに関する情報も取得することができます。

さらに、Clojureの`clojure.java.io`ネームスペースには、ファイルの各種操作やストリームの操作に便利な関数が多数用意されているので、ファイルシステムを操作する際には必ずチェックしてみることをお勧めします。

## さらに見る

- [Clojure 公式ドキュメント - `clojure.java.io`](https://clojure.github.io/clojure/clojure.java.io-api.html)
- [メジャーなライブラリ - `clojure.java.io`](https://mvnrepository.com/artifact/org.clojure/java.io)