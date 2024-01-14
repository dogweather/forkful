---
title:                "Clojure: JSONの扱い方"
simple_title:         "JSONの扱い方"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/working-with-json.md"
---

{{< edit_this_page >}}

# なぜJSONを使うのか？

JSONはあなたがよく使うデータ形式の一つです。データは通常テキスト形式で保存されるため、JSONを使わないと複雑な形式に変換する必要があります。JSONを使うことで、データを簡単に扱うことができ、プログラムやアプリケーションの統合も容易になります。

## 手順

```Clojure
(ns clojure-json-example.core
  (:require [clojure.data.json :as json]))

;; JSONデータのパース
(def json-data (json/read-str "{\"name\": \"John\", \"age\": 25, \"hobbies\": [\"reading\", \"cooking\"]}"))

;; データの取得
(def name (get json-data "name")) ;;=> "John"
(def age (get json-data "age"))   ;;=> 25
(def hobbies (get json-data "hobbies")) ;;=> ["reading", "cooking"]

;; データの作成
(def new-json-data (json/write-str {:name "Jane", :age 30, :hobbies ["writing", "singing"]}))
;;=> "{\"name\": \"Jane\", \"age\": 30, \"hobbies\": [\"writing\", \"singing\"]}"
```

## 深堀り

JSONを操作する際、キーワードを使うことでデータを取得したり作成したりすることができます。また、データをネストさせることもできます。Clojureには `clojure.data.json` ライブラリがあり、JSONデータのパースやシリアライズを簡単に行うことができます。

# 参考リンク

- [JSON with Clojure](https://clojure.org/reference/data_structures#_json)
- [clojure.data.json documentation](https://clojure.github.io/data.json/)