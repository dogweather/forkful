---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:27.593174-07:00
description: "\u65B9\u6CD5: Clojure\u306B\u306FJSON\u3092\u6271\u3046\u305F\u3081\u306E\
  \u7D44\u307F\u8FBC\u307F\u95A2\u6570\u304C\u542B\u307E\u308C\u3066\u3044\u306A\u3044\
  \u305F\u3081\u3001\u901A\u5E38\u306F\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u306E\
  \u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002`cheshire`\u3068\
  `jsonista`\u306F\u4F7F\u3044\u3084\u3059\u3055\u3068\u30D1\u30D5\u30A9\u30FC\u30DE\
  \u30F3\u30B9\u306E\u305F\u3081\u306B\u4EBA\u6C17\u306E\u3042\u308B\u9078\u629E\u80A2\
  \u3067\u3059\u3002"
lastmod: '2024-04-05T21:53:42.532549-06:00'
model: gpt-4-0125-preview
summary: "Clojure\u306B\u306FJSON\u3092\u6271\u3046\u305F\u3081\u306E\u7D44\u307F\u8FBC\
  \u307F\u95A2\u6570\u304C\u542B\u307E\u308C\u3066\u3044\u306A\u3044\u305F\u3081\u3001\
  \u901A\u5E38\u306F\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u306E\u30E9\u30A4\u30D6\
  \u30E9\u30EA\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002`cheshire`\u3068`jsonista`\u306F\
  \u4F7F\u3044\u3084\u3059\u3055\u3068\u30D1\u30D5\u30A9\u30FC\u30DE\u30F3\u30B9\u306E\
  \u305F\u3081\u306B\u4EBA\u6C17\u306E\u3042\u308B\u9078\u629E\u80A2\u3067\u3059\u3002"
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
