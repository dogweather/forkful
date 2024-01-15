---
title:                "「Jsonを扱う」"
html_title:           "Clojure: 「Jsonを扱う」"
simple_title:         "「Jsonを扱う」"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## なぜ

JSONは、データを簡潔かつ柔軟に表現できるため、多くのプログラミング言語でよく使われています。ClojureでもJSONを扱うことで、WebアプリケーションやAPIとの連携などに役立ちます。

## 方法

Clojureでは、JSONを扱うためのモジュールとして「Cheshire」があります。Cheshireを使用すると、複雑なデータ構造でも簡単に扱うことができます。例えば、以下のようにしてJSONを読み込み、オブジェクトとして扱うことができます。

```Clojure
(ns your-namespace
  (:require [cheshire.core :as json]))

;; JSONを読み込む
(def json-data (json/parse-string "{\"name\":\"Emily\", \"age\": 25}"))

;; オブジェクトとしてアクセスする
(println (:name json-data))
(println (:age json-data))

;; 出力
;; "Emily"
;; 25
```

また、Clojureでは簡単にJSONを生成することもできます。例えば、以下のようにClojureのデータ構造をJSONに変換することができます。

```Clojure
(ns your-namespace
  (:require [cheshire.core :as json]))

;; ClojureのハッシュマップをJSONに変換する
(json/generate-string {:name "Emily", :age 25})

;; 出力
;; "{\"name\":\"Emily\",\"age\":25}"
```

## 深堀り

Clojureのデータ構造とJSONの相互変換については、より詳細に知りたい場合、Cheshireのドキュメントを参照することをおすすめします。また、他のClojureのライブラリやツールと組み合わせることで、より効率的にJSONを扱うことができます。例えば、「Ring」というライブラリを使うことで、ClojureでWebアプリケーションを作成する際にJSONをどのように扱うかを学ぶことができます。

## 参考リンク

- Cheshire: https://github.com/dakrone/cheshire
- Ring: https://github.com/ring-clojure/ring