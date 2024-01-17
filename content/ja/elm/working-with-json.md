---
title:                "jsonとの作業"
html_title:           "Elm: jsonとの作業"
simple_title:         "jsonとの作業"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/working-with-json.md"
---

{{< edit_this_page >}}

## 何が、なぜ？

JSONを使ってプログラミングするとは、プログラマーがデータを受け取ったり送ったりする方法です。プログラムは、コンピューター間やネットワークを介してデータをやり取りする必要があり、その際にはJSONが役立ちます。

## 使い方：

```Elm
import Json.Decode exposing (decodeString, int, list, string)

-- 整数、文字列、リストという3つの要素を含むJSONデータ
jsonData = """{\"age\": 25, \"name\": \"Taro\", \"hobbies\": [\"reading\", \"coding\"]}"""

-- 与えられたJSONデータをデコードする
decodeString int ("age") jsonData
--> Ok 25

decodeString string ("name") jsonData
--> Ok "Taro"

decodeString (list string) ("hobbies") jsonData
--> Ok ["reading", "coding"]
```

## 深く掘り下げる：

JSONは1990年代初めに開発されたデータ記述形式で、その名前は"JavaScript Object Notation"の略です。他のデータフォーマットと比べて、JSONは人間にとっても機械にとっても読みやすく、広く利用されています。Elmプログラミング言語では、JSONをデコードするために内部的にはJSONファイルをJavaScriptオブジェクトに変換してから使います。

## その他の情報：

- [Elmの公式ドキュメント](https://guide.elm-lang.org/interop/json.html)にJSONを扱う方法が詳しく書かれています。
- [JSONフォーマットの歴史](https://www.json.org/json-ja.html)の詳細はJSONの公式サイトで見ることができます。