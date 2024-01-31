---
title:                "YAMLを扱う"
date:                  2024-01-19
html_title:           "Bash: YAMLを扱う"
simple_title:         "YAMLを扱う"

category:             "Clojure"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

YAMLは設定ファイルやデータの交換に使われる。読みやすく、書きやすいから、プログラマーはよく使う。

## How to: (やり方)

Clojureには`clj-yaml`ライブラリがある。これを使ってYAMLを扱おう。

```clojure
;; 依存関係を追加
;; [clj-yaml "0.7.0"]

(require '[clj-yaml.core :as yaml])

;; YAMLを文字列から読む
(def config-string "
http:
  port: 8080
  host: localhost
")

(def config (yaml/parse-string config-string))
config
;; => {:http {:port 8080, :host "localhost"}}

;; オブジェクトをYAMLに変換
(def config-map {:http {:port 8080, :host "localhost"}})

(yaml/generate-string config-map)
;; => "http:\n  host: \"localhost\"\n  port: 8080\n"
```

## Deep Dive (深掘り)

YAMLは"YAML Ain't Markup Language"の略で、2001年に発表された。JSONの代わりに使う人もいる。ClojureではJavaのSnakeYAMLライブラリに基づいた`clj-yaml`がある。速度や特定の機能が重要なら他のライブラリも検討する価値がある。

## See Also (関連情報)

- `clj-yaml` GitHubページ: https://github.com/clj-commons/clj-yaml
- YAML公式サイト: https://yaml.org
- SnakeYAML GitHubページ: https://github.com/asomov/snakeyaml
