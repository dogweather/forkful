---
title:                "「YAMLでの作業」"
html_title:           "Clojure: 「YAMLでの作業」"
simple_title:         "「YAMLでの作業」"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

## Why

なぜYAMLを使用するのか？YAMLはデータを人間が読みやすい形式で表現するためのファイル形式であり、プログラムコードで使用する際には便利です。また、YAMLはJSONよりも読み書きが容易で、設定ファイルやデータのやり取りに最適です。

## How To

YAMLをClojureで扱う方法を学ぶために、まずはClojureのプロジェクトを作成しましょう。

```Clojure
lein new app yaml-tutorial
```

次に、YAMLのデータを読み込むためにclj-yamlライブラリをプロジェクトに依存性として追加します。

```Clojure
(defproject yaml-tutorial "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/data.json "0.2.6"]
                 [clj-yaml "0.6.0"]])
```

プロジェクトの依存性を追加したら、コマンドラインで `lein repl` を実行してREPL環境を開き、YAMLデータを読み込むための準備をします。

```Clojure
(require '[clj-yaml.core :as yaml])
```

次に、YAMLファイルを読み込みます。ここでは簡単な例として、下記のようなYAMLファイルを作成します。

```YAML
# example.yaml

- name: John
  age: 25
  city: Tokyo
```

REPL環境で次のように入力すると、YAMLデータがClojureのデータ構造に変換され、変数に代入されます。

```Clojure
(def yaml-data (yaml/parse-file "example.yaml"))
```

変数 `yaml-data` に格納されたデータを確認すると、次のように表示されます。

```Clojure
=> ({:name "John", :age 25, :city "Tokyo"})
```

yamlデータを変更する場合は、 `yaml/generate` 関数を使用してyamlファイルに書き戻すことができます。

```Clojure
(require '[clojure.pprint :refer [pprint]])

;; yamlデータに新しい要素を追加
(conj yaml-data {:hobby "programming"})

;; yamlファイルにyamlデータを書き込む
(with-open [wrtr (io/writer "example.yaml")]
  (yaml/generate wrtr yaml-data))

;; 書き込んだyamlデータを確認
(pprint (yaml/parse-file "example.yaml"))
```

以上で、ClojureでのYAMLの基本的な読み書きができるようになりました。

## Deep Dive

### マッピング/シーケンス

YAMLはマッピングとシーケンスの2種類のコレクションをサポートしています。マッピングはキーと値のペアを持ち、シーケンスは順序付けられた要素のリストを保持します。これらのコレクションはClojureのデータ構造に変換される際にそれぞれ `hash-map` と `vector` に変換されます。

### クオートの使用

YAMLでは、値をクオートすることで文字列として認識させることができます。クオートはダブルクォーテーション `"` やシングルクォーテーション `'` を使用して表現することができます。

```YAML
# クオートを使用しな