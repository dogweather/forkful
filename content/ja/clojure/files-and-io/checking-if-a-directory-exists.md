---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:24.206345-07:00
description: "\u3069\u306E\u3088\u3046\u306B\uFF1A Clojure\u306FJVM\u8A00\u8A9E\u3067\
  \u3042\u308B\u305F\u3081\u3001\u3053\u306E\u76EE\u7684\u306E\u305F\u3081\u306BJava\u306E\
  `java.io.File`\u30AF\u30E9\u30B9\u3092\u5229\u7528\u3067\u304D\u307E\u3059\u3002\
  \u3053\u306E\u3088\u3046\u306A\u57FA\u672C\u7684\u306A\u64CD\u4F5C\u306B\u30B5\u30FC\
  \u30C9\u30D1\u30FC\u30C6\u30A3\u306E\u30E9\u30A4\u30D6\u30E9\u30EA\u306F\u5FC5\u8981\
  \u3042\u308A\u307E\u305B\u3093\u3002\u4EE5\u4E0B\u304C\u5B9F\u884C\u65B9\u6CD5\u3067\
  \u3059\uFF1A."
lastmod: '2024-04-05T22:37:49.907021-06:00'
model: gpt-4-0125-preview
summary: "\u3069\u306E\u3088\u3046\u306B\uFF1A Clojure\u306FJVM\u8A00\u8A9E\u3067\u3042\
  \u308B\u305F\u3081\u3001\u3053\u306E\u76EE\u7684\u306E\u305F\u3081\u306BJava\u306E\
  `java.io.File`\u30AF\u30E9\u30B9\u3092\u5229\u7528\u3067\u304D\u307E\u3059\u3002\
  \u3053\u306E\u3088\u3046\u306A\u57FA\u672C\u7684\u306A\u64CD\u4F5C\u306B\u30B5\u30FC\
  \u30C9\u30D1\u30FC\u30C6\u30A3\u306E\u30E9\u30A4\u30D6\u30E9\u30EA\u306F\u5FC5\u8981\
  \u3042\u308A\u307E\u305B\u3093\u3002\u4EE5\u4E0B\u304C\u5B9F\u884C\u65B9\u6CD5\u3067\
  \u3059\uFF1A."
title: "\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\u308B\u304B\u3069\
  \u3046\u304B\u306E\u78BA\u8A8D"
weight: 20
---

## どのように：
ClojureはJVM言語であるため、この目的のためにJavaの`java.io.File`クラスを利用できます。このような基本的な操作にサードパーティのライブラリは必要ありません。以下が実行方法です：

```clojure
(import 'java.io.File)

(defn directory-exists? [dir-path]
  (let [dir (File. dir-path)]
    (.exists dir)))

;; 使用例
(println (directory-exists? "/path/to/your/directory")) ;; true または false
```

この関数`directory-exists?`は、ディレクトリパスを文字列として受け取り、ディレクトリが存在する場合は`true`を、そうでない場合は`false`を返します。これは、ディレクトリパスで`File`オブジェクトを作成し、このオブジェクトに対して`.exists`メソッドを呼び出すことで実現されます。

Javaの直接の相互運用に加えて、Javaの雛形を隠すClojureのライブラリを使用できます。そのようなライブラリの1つが`clojure.java.io`です。しかし、ディレクトリが存在するかどうかを確認する場合は、まだ`File`クラスを使用しますが、他のファイル操作にはライブラリが役立つかもしれません。例：

```clojure
(require '[clojure.java.io :as io])

(defn directory-exists?-clojure [dir-path]
  (.exists (io/file dir-path)))

;; 使用例
(println (directory-exists?-clojure "/another/path/to/check")) ;; true または false
```

このバージョンはかなり似ていますが、`File`オブジェクトを作成するためにClojureの`io/file`関数を使用します。この方法は、Javaクラスと直接対話するのではなく、IO操作のためのClojureのライブラリを活用することで、Clojureのコードベースにより自然に溶け込みます。
