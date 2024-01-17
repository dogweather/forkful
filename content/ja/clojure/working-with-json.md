---
title:                "JSON を使ったコンピュータプログラミング"
html_title:           "Clojure: JSON を使ったコンピュータプログラミング"
simple_title:         "JSON を使ったコンピュータプログラミング"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## 何をして、なぜやるのか？

JSONとは、Java Script Object Notationの略で、データを保持するためのフォーマットです。プログラマーがJSONを使うのは、データを簡単に読み書きできるためです。

## 方法：

ClojureでJSONデータを扱う方法を説明します。まず、ClojureのライブラリであるCheshireを使い、JSONデータを読み込みます。

```Clojure
(def data (cheshire.core/parse-string "{\"name\":\"John\", \"age\":30}"))
```
dataという変数に、JSONデータがマップ形式で格納されます。

次に、JSONデータを書き込む方法を説明します。

```Clojure
(def json-string (cheshire.core/generate-string {:name "John", :age 30}))
```
json-stringという変数に、JSONデータが文字列として格納されます。

## 詳細を調べる：

JSONは、複数のプログラミング言語で使用されているデータフォーマットです。JSONは軽量で読み書きがしやすいため、ネットワーク上でデータをやりとりする際によく使われます。

JSONの代替としては、XMLやYAMLなどがありますが、それらよりもJSONの方が読み書きが簡単です。

Clojureでは、Cheshireの他にもClojure.data.jsonやjava.io.StringWriterなどのライブラリを使用してJSONデータを扱うことができます。

## 参考資料：

- [ClojureのCheshireライブラリ](https://github.com/dakrone/cheshire)
- [JSONの仕様書](https://www.json.org/json-ja.html)
- [XMLとの比較](https://www.safaribooksonline.com/library/view/head-first-servlets/9780596516680/ch04.html)