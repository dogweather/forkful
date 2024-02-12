---
title:                "YAML を操作する"
aliases:
- ja/clojure/working-with-yaml.md
date:                  2024-02-03T19:25:01.677774-07:00
model:                 gpt-4-0125-preview
simple_title:         "YAML を操作する"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

YAMLは、"YAML Ain't Markup Language"の再帰的頭字語であり、人間が読めるデータシリアライゼーション形式です。これは、設定ファイルや異なるデータ構造を持つ言語間のデータ交換に使用されます。プログラマーはYAMLのシンプルさと読みやすさを活用し、これをアプリケーションの設定や多言語プログラミング環境でのデータ交換を容易にする理想的な選択としています。

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
