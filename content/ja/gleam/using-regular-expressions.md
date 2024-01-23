---
title:                "正規表現の使用"
html_title:           "C: 正規表現の使用"
simple_title:         "正規表現の使用"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (何とその理由?)
正規表現とは、文字列のパターンを記述するための強力なツールです。プログラマーはこれを使って、検索、データ検証、置換などの処理を簡潔に行います。

## How to: (やり方)
Gleamで正規表現を使うには、`regex`パッケージが必要です。まず、正規表現をコンパイルし、それからマッチングを実施します。

```gleam
import gleam/regex

pub fn run() {
  let pattern = regex.regex("world").unwrap()
  let result = regex.find(pattern, "Hello, world!")
  case result {
    Ok(matches) -> "見つかりました！"
    Error(_) -> "見つかりませんでした。"
  }
}
```

サンプル出力: `見つかりました！`

## Deep Dive (深掘り)
正規表現は、1960年代に発明されました。`regex`パッケージは、バックトラッキングを避けるために正規表現をNFA(非決定性有限オートマトン)にコンパイルします。このアプローチは高性能です。正規表現の代替手段としては、文字列関数やパーサーがありますが、利便性が落ちます。

## See Also (関連情報)
- Gleamの `regex` パッケージ: https://hex.pm/packages/gleam_regex
- 正規表現のオンラインテスター: https://regex101.com/
- 正規表現のチュートリアル: https://www.regular-expressions.info/tutorial.html
