---
date: 2024-01-26 04:20:40.537264-07:00
description: "\u65B9\u6CD5\uFF1A Clojure\u3067TOML\u3092\u6271\u3046\u306B\u306F\u3001\
  `clj-toml`\u306E\u3088\u3046\u306A\u30E9\u30A4\u30D6\u30E9\u30EA\u304C\u5FC5\u8981\
  \u3067\u3059\u3002\u307E\u305A\u3001`deps.edn`\u306B\u305D\u308C\u3092\u8FFD\u52A0\
  \u3057\u307E\u3059\uFF1A."
lastmod: '2024-03-13T22:44:41.587464-06:00'
model: gpt-4-0125-preview
summary: "Clojure\u3067TOML\u3092\u6271\u3046\u306B\u306F\u3001`clj-toml`\u306E\u3088\
  \u3046\u306A\u30E9\u30A4\u30D6\u30E9\u30EA\u304C\u5FC5\u8981\u3067\u3059\u3002\u307E\
  \u305A\u3001`deps.edn`\u306B\u305D\u308C\u3092\u8FFD\u52A0\u3057\u307E\u3059\uFF1A\
  ."
title: "TOML\u3092\u6271\u3046\u65B9\u6CD5"
weight: 39
---

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
