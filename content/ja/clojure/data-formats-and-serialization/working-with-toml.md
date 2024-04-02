---
date: 2024-01-26 04:20:40.537264-07:00
description: "TOML\u3092\u6271\u3046\u3068\u3044\u3046\u3053\u3068\u306F\u3001\u8A2D\
  \u5B9A\u30D5\u30A1\u30A4\u30EB\u306B\u4EBA\u6C17\u304C\u3042\u308A\u3001\u305D\u306E\
  \u8AAD\u307F\u3084\u3059\u3055\u304B\u3089 \"Tom's Obvious, Minimal Language\" \u306E\
  \u6700\u5C0F\u9650\u306E\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3067\u30C7\u30FC\u30BF\
  \u3092\u6271\u3063\u3066\u3044\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u4EBA\u9593\u306B\u512A\u3057\
  \u3044\u30B7\u30F3\u30BF\u30C3\u30AF\u30B9\u3067\u305D\u306E\u307E\u307E\u52D5\u4F5C\
  \u3059\u308B\u76F4\u611F\u7684\u306A\u8A2D\u5B9A\u7BA1\u7406\u306E\u305F\u3081\u306B\
  \u3053\u308C\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.587464-06:00'
model: gpt-4-0125-preview
summary: "TOML\u3092\u6271\u3046\u3068\u3044\u3046\u3053\u3068\u306F\u3001\u8A2D\u5B9A\
  \u30D5\u30A1\u30A4\u30EB\u306B\u4EBA\u6C17\u304C\u3042\u308A\u3001\u305D\u306E\u8AAD\
  \u307F\u3084\u3059\u3055\u304B\u3089 \"Tom's Obvious, Minimal Language\" \u306E\u6700\
  \u5C0F\u9650\u306E\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3067\u30C7\u30FC\u30BF\u3092\
  \u6271\u3063\u3066\u3044\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u4EBA\u9593\u306B\u512A\u3057\u3044\
  \u30B7\u30F3\u30BF\u30C3\u30AF\u30B9\u3067\u305D\u306E\u307E\u307E\u52D5\u4F5C\u3059\
  \u308B\u76F4\u611F\u7684\u306A\u8A2D\u5B9A\u7BA1\u7406\u306E\u305F\u3081\u306B\u3053\
  \u308C\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002"
title: "TOML\u3092\u6271\u3046\u65B9\u6CD5"
weight: 39
---

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
