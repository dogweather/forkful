---
title:                "「Jsonでの作業」"
html_title:           "Fish Shell: 「Jsonでの作業」"
simple_title:         "「Jsonでの作業」"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?

JSONとは何かを説明し、プログラマーがJSONを使用する理由を述べます。
JSONは、データを簡単に読み書きできるフォーマットで、略式でのみ使用されることが多いです。プログラマーは、JSONを使用することでデータの伝送や保存を容易にすることができます。

## How to:

Fish Shellを使ってJSONを扱う方法を説明します。

```
# JSON形式のデータを作成します
set data '{"name": "John", "age": 32, "city": "Tokyo"}'

# JSONをパースして、コンソールに表示します
echo $data | fromjson

# 変更したデータを再度JSON形式に変換します
set changedData (echo $data | fromjson | jq '.age = 33' | tojson)

#新しいデータを表示します
echo $changedData
```

## Deep Dive

JSONはJavaScript Object Notationの略です。元々JavaScriptのデータ型として開発されましたが、現在では多くのプログラミング言語で使用されています。しかし、JSONにはいくつかの代替方法があります。例えば、XMLやYAMLなどがあります。しかし、JSONはシンプルであるため、多くのプログラマーはJSONを好んで使用します。

JSONは、データを読み書きするためのコマンドラインツールであるjqを使用することで操作することができます。jqは、パイプラインを使用することでJSONデータを非常に簡単に処理できるように設計されています。また、tojsonコマンドを使用することで、任意のデータをJSON形式に変換することができます。

## See Also

- [jq公式サイト](https://stedolan.github.io/jq/)
- [JSON 入門 - MDN](https://developer.mozilla.org/ja/docs/Learn/JavaScript/Objects/JSON)