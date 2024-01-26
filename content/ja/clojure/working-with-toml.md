---
title:                "TOMLを扱う方法"
date:                  2024-01-26T04:20:40.537264-07:00
model:                 gpt-4-0125-preview
simple_title:         "TOMLを扱う方法"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/working-with-toml.md"
---

{{< edit_this_page >}}

## 何とその理由？
TOMLを扱うということは、設定ファイルに人気があり、その読みやすさから "Tom's Obvious, Minimal Language" の最小限のフォーマットでデータを扱っていることを意味します。プログラマーは、人間に優しいシンタックスでそのまま動作する直感的な設定管理のためにこれを使用します。

## 方法：
ClojureでTOMLを扱うには、`clj-toml`のようなライブラリが必要です。まず、`deps.edn`にそれを追加します：

```clojure
{:deps {clj-toml {:mvn/version "0.5.0"}}}
```

次に、TOMLをパースします：

```clojure
(require '[clj-toml.core :as toml])

(def config-str "title = 'TOML Example'")

(def parsed-config (toml/parse-string config-str))

;; パースされたTOMLからタイトルを取得
(println (:title parsed-config)) ;; 出力: TOML Example
```

TOMLを生成する場合：

```clojure
(def data {:title "TOML Example"})

(println (toml/generate-string data))
;; 出力: title = "TOML Example"
```

## 深掘り
TOMLは2013年ごろにGitHubの共同設立者であるTom Preston-Wernerによって、設定ファイルのためのYAMLやJSONよりもシンプルな代替物として作られました。これは明確性を目指し、追加のツールなしで人間が読める仕様であることを意図しています。

JSONはしばしばAPIやWebアプリケーション用に使用され、YAMLは参照やスクリプト機能で複雑になり得ますが、TOMLはシンプルな、テーブルベースの構造に焦点を当てて際立っています。このシンプルさは、Rustコミュニティや他の現代言語環境で特に人気があります。

Clojureは、そのシンプルさと実用性に重点を置いているため、設定でTOMLとうまく組み合わせることができます。`clj-toml`や代替ライブラリは、隙間を埋めます。これらはTOMLの静的データをClojureの動的で関数的な世界に変換します。

## 参照
- TOMLのGitHubリポジトリ：[github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- `clj-toml` on Clojars: [clojars.org/clj-toml](https://clojars.org/clj-toml)
- Clojure文書：[clojure.org](https://clojure.org/guides/getting_started)
- `clj-toml`の紹介：[github.com/lantiga/clj-toml](https://github.com/lantiga/clj-toml)