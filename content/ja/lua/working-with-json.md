---
title:                "「Jsonの取り扱い」"
html_title:           "Lua: 「Jsonの取り扱い」"
simple_title:         "「Jsonの取り扱い」"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/working-with-json.md"
---

{{< edit_this_page >}}

タイトル: LuaでJSONデータを処理する方法

## 何をするのか？ ##
JSONとは、JavaScript Object Notationの略で、データを扱うためのフォーマットの一つです。プログラマーは主に、データをサーバーやクライアント間で簡単に交換するためにJSONを使用します。

## 使い方 ##
Luaでは、JSONのデータを扱うための専用ライブラリがあります。まずはこのライブラリをプログラムに読み込む必要があります。

```Lua
local json = require("json")
```

次に、JSONデータをパースしてLuaのテーブル型に変換する必要があります。これには`json.parse()`メソッドを使用します。

```Lua
local data = json.parse('{"name":"John","age":30,"city":"New York"}')
print(data.name) -- John
print(data.age) -- 30
print(data.city) -- New York
```

そして、Luaのテーブル型をJSONデータに変換するには`json.stringify()`メソッドが使えます。

```Lua
local table = {name = "John", age = 30, city = "New York"}
print(json.stringify(table)) -- {"name":"John","age":30,"city":"New York"}
```

## 詳しく見ていく ##
JSONは、もともとJavaScriptで使われるために作られたフォーマットですが、現在では多くのプログラミング言語でサポートされています。代表的なものとしては、XMLやYAMLがありますが、JSONはいずれよりもシンプルで扱いやすく、Web開発やAPI開発に特に適しています。

Luaでは、JSONデータを扱うためには上記の方法以外にも、``yaml``や``dkjson``といったライブラリを使用することもできます。また、JSONの実装方法として、「object-style」（オブジェクト形式）と「array-style」（配列形式）の二つがあります。詳しくは公式ドキュメントを参照してください。

## 関連リンク ##
- [Lua公式ドキュメント](http://www.lua.org/pil/16.html)
- [JSONの公式サイト](https://www.json.org/json-ja.html)