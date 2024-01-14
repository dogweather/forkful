---
title:                "Clojure: 「yamlを扱う」"
simple_title:         "「yamlを扱う」"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

## なぜ

ClojureにおけるYAMLのプログラミングに携わる理由は、データのシリアライズとデシリアライズを行う上で、簡潔で直感的な方法を提供するためです。

## 方法

YAMLをClojureで使用する最も簡単な方法は、YAMLプラグインを利用することです。以下のコード例をご覧ください。

```Clojure
(require '[clj-yaml.core :as yaml])

;; YAMLのデータをClojureのマップにロードする
(def data (yaml/read-string "name: John Doe, age: 30, hobbies: [hiking, cooking, reading]"))

;; データを操作する
(println (str "名前: " (:name data))) ; 結果: 名前: John Doe
(println (str "年齢: " (:age data))) ; 結果: 年齢: 30
(println (str "趣味: " (clojure.string/join ", " (:hobbies data)))) ; 結果: 趣味: hiking, cooking, reading

;; ClojureのマップをYAMLのデータにエンコードする
(def yaml-data (yaml/write-string {:name "Jane Smith", :age 25, :hobbies ["painting", "dancing", "traveling"]}))
(println yaml-data) ; 結果: name: Jane Smith, age: 25, hobbies: [painting, dancing, traveling]
```

## ディープダイブ

YAMLは、データの構造を保持し、コンフィグファイルやデータベースなど、さまざまな形式のファイル間でのデータの交換に最適なフォーマットです。さらに、Clojureとの統合にも優れており、柔軟性と効率性を高めることができます。

## 関連情報

- [clojure-yamlライブラリのドキュメント](https://github.com/yogthos/clj-yaml)
- [YAMLでデータを操作するClojureのチュートリアル](https://purelyfunctional.tv/guide/read-yaml-files-in-clojure/)
- [ClojureのYAMLプラグインの紹介記事](https://quanttype.net/posts/2014-08-17-introducing-clojure-yaml.html)