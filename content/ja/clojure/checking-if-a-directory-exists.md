---
title:    "Clojure: ディレクトリが存在するかどうかをチェックする"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## なぜ

ディレクトリが存在するかどうかを確認することの重要性は、プログラミングの世界では非常に一般的です。例えば、あなたがファイルを読み込もうとするとき、そのファイルが本当に存在することを確認する必要があります。あるいは、特定のディレクトリが存在しているかどうかを確認して、条件分岐を行う必要があるかもしれません。このような場面では、ディレクトリが存在するかどうかを確認することが重要です。

## 方法

Clojureを使ってディレクトリが存在するかどうかを確認するには、`fs/exists?`関数を使用します。この関数は、引数としてチェックするディレクトリのパスを受け取り、ディレクトリが存在する場合は`true`、存在しない場合は`false`を返します。以下の例をご覧ください。

```Clojure
(ns example.core
  (:require [clojure.java.io :as io]
            [clojure.java.io :refer [file]]))

(defn check-directory? [dir]
  (fs/exists? (io/file dir)))

(defn -main []
  (let [dir (io/file "sample/directory")]
    (println (check-directory? dir))))

;; 出力: true
```

## 詳細説明

`fs/exists?`関数は、指定されたディレクトリだけでなく、その子ディレクトリも確認することができます。また、ファイルではなくディレクトリを指定した場合、`exists?`関数は常に`true`を返します。

この関数を使用してディレクトリを確認する際には、まず`clojure.java.io`ライブラリを使用して`io/file`関数を使ってパスを作成する必要があります。また、プロジェクト内のディレクトリを指定する際は、`(io/file "sample/directory")`のように、`project-name/resources`からの相対パスを指定することができます。

## おさらい

ディレクトリが存在するかどうかを確認する際には、Clojureの`fs/exists?`関数を使用することができます。`fs/exists?`はディレクトリだけでなく、子ディレクトリも確認することができます。また、プロジェクト内のディレクトリを指定するには、相対パスを使用することができます。

## 参考リンク

- [ClojureDoc: fs/exists?](https://clojuredocs.org/clojure.java.io/fs/exists_q)
- [Clojure for the Brave and True](http://www.braveclojure.com/files/io-intro.zip) by Daniel Higginbotham.