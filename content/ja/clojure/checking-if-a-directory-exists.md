---
title:                "Clojure: ディレクトリが存在するかどうかの確認"
simple_title:         "ディレクトリが存在するかどうかの確認"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜ

ディレクトリが存在するかどうかを確認したいと思う理由はさまざまですが、一般的には、プログラミングの中でファイルやディレクトリを処理する際に、それらが存在するかどうかを確認する必要があるからです。ディレクトリが存在するかどうかを確認することで、不必要なエラーを防ぐことができ、プログラムの安定性を保つことができます。

## 作り方

まずは、Clojureの標準ライブラリである`clojure.java.io`を使用して、ディレクトリを表すオブジェクトを作成します。

```Clojure
(require '[clojure.java.io :as io])

(def dir (io/file "path/to/directory"))
```

次に、`.exists`メソッドを使用して、ディレクトリが存在するかどうかを確認できます。

```Clojure
(.exists dir) ; => true or false
```

また、さまざまな条件でディレクトリが存在するかどうかを確認することもできます。例えば、指定したパスがファイルでなくディレクトリであるかどうかを確認することもできます。

```Clojure
(.isDirectory dir) ; => true or false
```

## ディープダイブ

Clojureでディレクトリの存在を確認するには、Javaの標準ライブラリである`File`クラスを使用することができます。この`File`クラスは、ファイルやディレクトリを表すオブジェクトを作成し、そのオブジェクトに対して`exists`や`isDirectory`などのメソッドを呼び出すことで、ディレクトリの存在を確認することができます。また、`-io`ライブラリを使用することで、より簡潔にディレクトリの存在を確認することができます。

## See Also

- [ClojureDocs - clojure.java.io](https://clojuredocs.org/clojure.java.io)
- [Oracle - File class](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/File.html)
- [Clojureverse - Working with Directories and Files](https://clojureverse.org/t/working-with-directories-and-files/2216)