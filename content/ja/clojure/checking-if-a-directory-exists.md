---
title:                "ディレクトリが存在するか確認する方法"
html_title:           "Clojure: ディレクトリが存在するか確認する方法"
simple_title:         "ディレクトリが存在するか確認する方法"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

##なぜ

あなたは「ディレクトリが存在するかどうかをチェックする必要があるかもしれない」と思うかもしれません。例えば、あなたが自分のプログラムで特定の操作を実行する前に、そのディレクトリが存在することを確認する必要があるかもしれません。

##やり方

 Clojureには、ディレクトリが存在するかどうかをチェックするための便利な関数があります。それは`java.io.File`を使用することで実現することができます。以下の例を見てみましょう。

```
Clojure
(defn check-directory [path]
  (let [file (java.io.File. path)]
    (.isDirectory file)))

(check-directory "/home/username/directory")
```

上記のコードは、与えられたパスが存在するディレクトリであれば`true`を返し、そうでなければ`false`を返します。

##深堀り

しかし、`java.io.File`の`isDirectory`関数は、与えられたパスが実際に存在するかどうかをチェックすることはできません。そのため、パスが存在しない場合でも`false`が返されます。そのため、先ほどの例では、パスが実際に存在するかどうかをチェックするための追加のコードが必要になります。 例えば、以下のようにすることで、パスが存在するかどうかを確認することができます。

```
Clojure
(defn check-directory [path]
  (let [file (java.io.File. path)]
    (and (.exists file) (.isDirectory file))))

(check-directory "/home/username/nonexistent_directory")
```

上記の例では、パスが存在しないため、`false`が返されます。

##参考リンク

- [ClojureDocs: java.io.File](https://clojuredocs.org/clojure.java.io/file)
- [ClojureDocs: .exists](https://clojuredocs.org/clojure.java.io/exists%21)
- [ClojureDocs: .isDirectory](https://clojuredocs.org/clojure.java.io/is-directory%3F)