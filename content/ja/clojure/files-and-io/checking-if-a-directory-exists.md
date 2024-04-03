---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:24.206345-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.577410-06:00'
model: gpt-4-0125-preview
summary: "Clojure\u3067\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\
  \u308B\u304B\u3069\u3046\u304B\u3092\u78BA\u8A8D\u3059\u308B\u3053\u3068\u306F\u3001\
  Clojure\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u5185\u304B\u3089\u30D5\u30A1\
  \u30A4\u30EB\u30B7\u30B9\u30C6\u30E0\u306E\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u306E\
  \u5B58\u5728\u3092\u691C\u8A3C\u3059\u308B\u3053\u3068\u3092\u542B\u307F\u307E\u3059\
  \u3002\u3053\u306E\u30BF\u30B9\u30AF\u306F\u30D5\u30A1\u30A4\u30EB\u64CD\u4F5C\u306B\
  \u3068\u3063\u3066\u91CD\u8981\u3067\u3001\u5B58\u5728\u3057\u306A\u3044\u304B\u3082\
  \u3057\u308C\u306A\u3044\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304B\u3089\u8AAD\u307F\
  \u53D6\u308A\u3084\u66F8\u304D\u8FBC\u307F\u3092\u884C\u3046\u969B\u306B\u30A8\u30E9\
  \u30FC\u3092\u9632\u304E\u3001\u5805\u7262\u3067\u30A8\u30E9\u30FC\u306E\u306A\u3044\
  \u30B3\u30FC\u30C9\u5B9F\u884C\u3092\u78BA\u5B9F\u306B\u3059\u308B\u305F\u3081\u3067\
  \u3059\u3002."
title: "\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\u308B\u304B\u3069\
  \u3046\u304B\u306E\u78BA\u8A8D"
weight: 20
---

## 何となぜ？
Clojureでディレクトリが存在するかどうかを確認することは、Clojureアプリケーション内からファイルシステムのディレクトリの存在を検証することを含みます。このタスクはファイル操作にとって重要で、存在しないかもしれないディレクトリから読み取りや書き込みを行う際にエラーを防ぎ、堅牢でエラーのないコード実行を確実にするためです。

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
