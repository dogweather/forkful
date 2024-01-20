---
title:                "ディレクトリが存在するかどうかを確認する"
html_title:           "Clojure: ディレクトリが存在するかどうかを確認する"
simple_title:         "ディレクトリが存在するかどうかを確認する"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 何となぜ？

ディレクトリの存在確認は、指定したパスにディレクトリが存在するかどうかを確認する処理です。プログラマーがこれを行う理由は、ファイルの読み書きを安全に行い、エラーや他の問題を避けるためです。

## 実装方法:

```clojure
(require '[clojure.java.io :as io])

(defn dir-exists?
  [dir-path]
  (let [dir-file (io/file dir-path)]
    (.exists dir-file)))
```

使用例: 

```clojure
(dir-exists? "/path/to/directory")
; 出力: true または false
(dir-exists? "/path/that/does/not/exist")
; 出力: false 
```

## 詳細

特定のディレクトリの存在を確認するという問題は、ファイルシステムの利用が初めて可能になった時から存在しています。たとえば、一部のプログラミング言語では `os.path` モジュールの `exists()`関数を使用し、ディレクトリの存在を確認することができます。

しかし Clojureでは、Java interopを使用してこれを実現しています。ClojureはJava上で動作するため、Javaのライブラリとメソッドを利用することができます。

具体的には、Clojure の `clojure.java.io/file` 関数を使用して、指定したパスからJavaの `File` オブジェクトを作成します。その `File` オブジェクトに対して `.exists` メソッドを呼び出すと、そのディレクトリが存在するかどうかが返されます。

## 参考資料

- ClojureのJava interopについて: [https://clojure.org/reference/java_interop](https://clojure.org/reference/java_interop)
- Javaの `File` クラスについて: [https://docs.oracle.com/javase/7/docs/api/java/io/File.html](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
- ファイルとディレクトリの操作についてのClojureのガイド: [https://clojure.org/guides/io](https://clojure.org/guides/io)