---
title:                "yamlを扱う"
html_title:           "Clojure: yamlを扱う"
simple_title:         "yamlを扱う"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAMLとは何か？
YAMLとは、人間にとって読みやすいデータシリアライズ言語の一つです。プログラマーは構造化されたデータをファイルに保存したり、ネットワークで受け渡したりする際に、よく使用します。

## How to:
コーディング例と出力
```
Clojureコードブロックを使用して、YAMLデータを作成する方法を説明します。
```
(```Clojure)  
(require '[clojure.data.yaml :as yaml])  
(def data {:name "John" :age 25})  
(yaml/write-str data)  
```

```  
出力: "---\nage: 25\nname: John" 

## Deep Dive:
深層情報
YAMLのルーツは、プログラム言語であるPerlのモジュールである"YAML"から来ており、2001年に最初にリリースされました。代替としては、JSONやXMLがありますが、YAMLは読みやすさと表現力で優れています。YAMLは、様々なプログラミング言語でサポートされており、また、設定ファイルやデータ転送に広く使用されています。Clojureでは、YAMLの読み書きに"Clojure.data.yaml"ライブラリを使用することができます。

## See Also:
関連リソースへのリンク
- [Official YAML website](https://yaml.org/)
- [Clojure.data.yaml official documentation](https://clojure.github.io/data.json/)
- [Comparison of YAML, JSON and XML](https://www.yaml.info/comparison/)