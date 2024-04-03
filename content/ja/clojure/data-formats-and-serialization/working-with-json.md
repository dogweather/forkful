---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:27.593174-07:00
description: "Clojure\u3067JSON\uFF08JavaScript Object\u2026"
lastmod: '2024-03-13T22:44:41.585434-06:00'
model: gpt-4-0125-preview
summary: "Clojure\u3067JSON\uFF08JavaScript Object Notation\uFF09\u3092\u6271\u3046\
  \u3053\u3068\u306F\u3001JSON\u6587\u5B57\u5217\u3092Clojure\u306E\u30C7\u30FC\u30BF\
  \u69CB\u9020\uFF08\u30DE\u30C3\u30D7\u3001\u30D9\u30AF\u30BF\u30FC\uFF09\u306B\u89E3\
  \u6790\u3057\u3001\u305D\u306E\u9006\u3092\u884C\u3046\u4F5C\u696D\u3092\u542B\u307F\
  \u307E\u3059\u3002\u3053\u306E\u30BF\u30B9\u30AF\u306F\u3001\u30C7\u30FC\u30BF\u3092\
  \u69CB\u9020\u5316\u3055\u308C\u305F\u30C6\u30AD\u30B9\u30C8\u30D9\u30FC\u30B9\u306E\
  \u5F62\u5F0F\u3067\u901A\u4FE1\u3059\u308B\u5FC5\u8981\u304C\u3042\u308BWeb\u30B5\
  \u30FC\u30D3\u30B9\u3001API\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\
  \u306B\u3068\u3063\u3066\u57FA\u672C\u7684\u3067\u3059\u3002\u306A\u305C\u306A\u3089\
  \u3001JSON\u306F\u7570\u306A\u308B\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u74B0\
  \u5883\u5168\u4F53\u3067\u666E\u904D\u7684\u306B\u8A8D\u8B58\u3055\u308C\u3001\u30B5\
  \u30DD\u30FC\u30C8\u3055\u308C\u3066\u3044\u308B\u304B\u3089\u3067\u3059\u3002."
title: "JSON\u3092\u6D3B\u7528\u3059\u308B"
weight: 38
---

## 方法:
ClojureにはJSONを扱うための組み込み関数が含まれていないため、通常はサードパーティのライブラリを使用します。`cheshire`と`jsonista`は使いやすさとパフォーマンスのために人気のある選択肢です。

### Cheshireの使用
まず、`project.clj`のプロジェクト依存関係にCheshireを追加します：
```clj
[com.fasterxml.jackson.core/jackson-core "2.12.0"]
[cheshire "5.10.1"]
```

JSON文字列をClojureマップに解析し、マップをJSON文字列に変換するには：

```clj
(require '[cheshire.core :as json])

;; JSON文字列をClojureマップに解析
(let [json-input "{\"name\":\"John\", \"age\":30}"]
  (json/parse-string json-input true)) ; => {"name" "John", "age" 30}

;; ClojureマップをJSON文字列に変換
(let [clj-map {"name" "John", "age" 30}]
  (json/generate-string clj-map)) ; => "{\"name\":\"John\",\"age\":30}"
```

### Jsonistaの使用
プロジェクトの`project.clj`にJsonistaを追加：
```clj
[jsonista "0.3.2"]
```

Jsonistaでの同様の操作：

```clj
(require '[jsonista.core :as j])

;; JSON文字列をClojureに解析
(let [json-input "{\"name\":\"Emily\", \"age\":25}"]
  (j/read-value json-input)) ; => {"name" "Emily", "age" 25}

;; ClojureマップをJSON文字列に変換
(let [clj-map {"name" "Emily", "age" 25}]
  (j/write-value-as-string clj-map)) ; => "{\"name\":\"Emily\",\"age\":25}"
```

どちらのライブラリも、より複雑なデータ構造のエンコードとデコードのオプションがあり、シリアライゼーションとデシリアライゼーションプロセスのカスタマイズを可能にする追加の関数とパラメーターがあります。ほとんどのアプリケーションにとって、示された機能はClojureアプリケーションでのJSONの扱いのための堅固な基盤を提供します。
