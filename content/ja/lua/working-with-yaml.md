---
title:                "Yamlを使う。"
html_title:           "Lua: Yamlを使う。"
simple_title:         "Yamlを使う。"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/working-with-yaml.md"
---

{{< edit_this_page >}}

YAMLを使用すること

## なにそれ
YAMLはデータの表現を容易にするために作られた、人間にとって読みやすいデータ形式です。プログラマーはYAMLを使用することで、より簡単にデータを作成、保存、共有することができます。

## 使い方
```Lua
-- YAMLファイルを読み込み、変数に保存する
local yaml = require("yaml")
file = io.open("input.yaml", "r")
data = yaml.load(file)
file:close()

-- データをYAML形式で書き出す
output = yaml.dump(data)
```

## 詳細を知る
YAMLは2001年に作られ、Pythonのデータ形式である「YAML Ain't Markup Language」の略称から名付けられました。代表的な代替形式としてJSONがありますが、YAMLはより読みやすく、より複雑なデータ構造にも対応できるという特徴があります。YAMLの実装には、Luaで使用できるlua-yamlモジュールがあります。

## 関連情報
- 詳しい使用方法や仕様については、公式ドキュメントを参照してください。 https://yaml.org/spec/
- YAMLをPythonで使用する方法については、Python公式ドキュメントを参照してください。 https://docs.python.org/3/library/yaml.html