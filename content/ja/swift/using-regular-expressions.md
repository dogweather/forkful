---
title:                "正規表現を使用する"
html_title:           "Swift: 正規表現を使用する"
simple_title:         "正規表現を使用する"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 何 & 何故？
正規表現を使うとは何か？それをしている理由はなんですか？
正規表現は、文字列内で特定のパターンを検索するためのツールです。プログラマーは、データ処理やバリデーションなど、さまざまな目的で正規表現を使用します。

## 方法：
```Swift
let str = "Hello, world!"
let pattern = "Hello"

if let range = str.range(of: pattern) {
    print("パターンが見つかりました。")
} else {
    print("パターンは見つかりませんでした。")
}
```
上記のコードでは、文字列内で "Hello" というパターンを検索し、見つかった場合にメッセージを表示します。

## 深堀り：
正規表現は、1960年代に誕生したテキスト処理の技術です。しかし、シンプルな検索や置換の機能を持つ正規表現に代わるより高度なテキスト処理ツールもあります。実装の詳細に関しては、正規表現エンジンの種類や、各言語でのサポートの違いなどがあります。

## 関連リンク：
1. [正規表現チュートリアル (英語)](https://regexone.com/)
2. [正規表現オンラインテストツール (英語)](https://regexr.com/)