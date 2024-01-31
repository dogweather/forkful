---
title:                "JSONを扱う方法"
date:                  2024-01-19
html_title:           "Arduino: JSONを扱う方法"
simple_title:         "JSONを扱う方法"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
JSONはデータ交換のフォーマット。ClojureでJSONを扱うことは、ウェブアプリやAPIとのデータ交換が必要な時に重要。

## How to: (方法)
Clojureには`cheshire`というライブラリがよく使われる。JSONのエンコードとデコードが簡単にできる。

```Clojure
;; 依存関係の追加
;; [cheshire "5.10.1"] ;project.cljに追加またはdeps.ednに対応する形で

(require '[cheshire.core :as json])

;; JSONエンコード
(json/encode {:foo "bar" :num 42})
;; => "{\"foo\":\"bar\",\"num\":42}"

;; JSONデコード
(json/decode "{\"foo\":\"bar\",\"num\":42}" true)
;; => {:foo "bar", :num 42}
```

## Deep Dive (掘り下げ)
1999年にJSONが登場。軽量で読みやすく、JavaScriptとの親和性が高い。ClojureでのJSON処理には`cheshire`以外にも`clojure.data.json`があるが、機能面では`cheshire`の方が一般的に優れているとされる。

## See Also (関連情報)
- [Cheshire GitHub](https://github.com/dakrone/cheshire): Cheshireライブラリ
- [clojure.data.json GitHub](https://github.com/clojure/data.json): clojure.data.jsonライブラリ
- [Clojure公式サイト](https://clojure.org): Clojureに関するさらなる情報
- [JSON公式サイト](https://www.json.org/json-en.html): JSONの詳細
