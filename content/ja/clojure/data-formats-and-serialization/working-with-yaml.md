---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:01.677774-07:00
description: "\u65B9\u6CD5\uFF1A Clojure\u306FYAML\u306E\u7D44\u307F\u8FBC\u307F\u30B5\
  \u30DD\u30FC\u30C8\u3092\u542B\u3093\u3067\u3044\u307E\u305B\u3093\u304C\u3001`clj-yaml`\u306E\
  \u3088\u3046\u306A\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u30E9\u30A4\u30D6\u30E9\
  \u30EA\u3092\u5229\u7528\u3057\u3066YAML\u30C7\u30FC\u30BF\u306E\u89E3\u6790\u3084\
  \u751F\u6210\u3092\u884C\u3046\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\u307E\
  \u305A\u3001\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\
  \u306E\u4F9D\u5B58\u95A2\u4FC2\u306B\u8FFD\u52A0\u3057\u307E\u3059\uFF1A."
lastmod: '2024-04-05T21:53:42.531205-06:00'
model: gpt-4-0125-preview
summary: ''
title: "YAML \u3092\u64CD\u4F5C\u3059\u308B"
weight: 41
---

## 方法：
ClojureはYAMLの組み込みサポートを含んでいませんが、`clj-yaml`のようなサードパーティライブラリを利用してYAMLデータの解析や生成を行うことができます。まず、ライブラリをプロジェクトの依存関係に追加します：

```clojure
;; これをプロジェクトのproject.cljの依存関係に追加
[clj-yaml "0.7.0"]
```

以下は、`clj-yaml`を使用してYAMLを解析し、ClojureのマップをYAMLに変換する方法です。

### YAMLの解析：
```clojure
(require '[clj-yaml.core :as yaml])

;; YAML文字列の解析
(let [yaml-str "name: John Doe\nage: 30\nlanguages:\n  - Clojure\n  - Python"]
  (yaml/parse-string yaml-str))
;; 出力：
;; => {"name" "John Doe", "age" 30, "languages" ["Clojure" "Python"]}
```

### ClojureからYAMLへの生成：
```clojure
(require '[clj-yaml.core :as yaml])

;; ClojureのマップをYAML文字列に変換
(let [data-map {:name "Jane Doe" :age 28 :languages ["Java" "Ruby"]}]
  (yaml/generate-string data-map))
;; 出力：
; "age: 28\nlanguages:\n- Java\n- Ruby\nname: Jane Doe\n"
```

これらの`clj-yaml`での簡単な操作は、Clojureアプリケーションに統合され、設定ファイルの処理やYAMLを使用する他のサービスやコンポーネントとのデータ交換を容易にすることができます。
