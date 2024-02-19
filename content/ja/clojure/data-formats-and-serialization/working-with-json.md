---
aliases:
- /ja/clojure/working-with-json/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:27.593174-07:00
description: "Clojure\u3067JSON\uFF08JavaScript Object\u2026"
lastmod: 2024-02-18 23:08:54.623514
model: gpt-4-0125-preview
summary: "Clojure\u3067JSON\uFF08JavaScript Object\u2026"
title: "JSON\u3092\u6D3B\u7528\u3059\u308B"
---

{{< edit_this_page >}}

## 何となく理由
ClojureでJSON（JavaScript Object Notation）を扱うことは、JSON文字列をClojureのデータ構造（マップ、ベクター）に解析し、その逆を行う作業を含みます。このタスクは、データを構造化されたテキストベースの形式で通信する必要があるWebサービス、API、アプリケーションにとって基本的です。なぜなら、JSONは異なるプログラミング環境全体で普遍的に認識され、サポートされているからです。

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
