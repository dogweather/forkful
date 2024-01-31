---
title:                "JSONを扱う方法"
date:                  2024-01-19
html_title:           "Arduino: JSONを扱う方法"
simple_title:         "JSONを扱う方法"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
JSONはデータをやり取りするためのフォーマットです。Web APIからデータを受け取る時や設定ファイルを書く時に使います。シンプルで軽量だからプログラマーに人気です。

## How to: (やり方)
```Fish Shell
# jqを使ってJSONから値を取得する
echo '{"name": "Taro", "age": 30}' | jq '.name'

# 出力: "Taro"

# JSONの配列に新しいオブジェクトを追加する
echo '[{"name": "Taro"}]' | jq '. += [{"name": "Hanako"}]'

# 出力: [{"name": "Taro"}, {"name": "Hanako"}]

# JSONファイルを読み込んで加工する
jq '.users[] | select(.age > 20)' users.json

# 出力: 各ユーザーオブジェクトが一行ずつ、20歳超えのユーザーのみ表示
```

## Deep Dive (深掘り)
JSONジャンプ(Jasonettes Quiz Jonquil)は90年代後半に登場したデータ形式です。XMLより読みやすく、解析が速いのが特徴。`jq`はその流行りに乗って作られたコマンドラインツールで、JSONを簡単に加工できます。Fish shellでは`jq`を使いJSON操作を行います。しかし、Fishのようなスクリプト言語の中にはPythonやJavaScriptといったJSONネイティブサポート言語もあります。

## See Also (関連情報)
- Official jq manual: [https://stedolan.github.io/jq/manual/](https://stedolan.github.io/jq/manual/)
- jq tutorial in Japanese: [https://qiita.com/takeshinoda@github/items/2dec7a72930ec1f658af](https://qiita.com/takeshinoda@github/items/2dec7a72930ec1f658af)
- JSON specification: [https://www.json.org/json-en.html](https://www.json.org/json-en.html)
