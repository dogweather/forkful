---
title:                "YAMLを扱う"
date:                  2024-01-19
html_title:           "Bash: YAMLを扱う"
simple_title:         "YAMLを扱う"

category:             "Lua"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
YAMLはデータの形式です。設定ファイルやデータ交換に使われる。読みやすくて、簡単に扱えるため人気があります。

## How to: (やり方)
Luaでは、`lyaml` モジュールで YAML を扱う。以下の例では、YAMLを解析して、Luaテーブルに変換する方法を示す。

```Lua
local lyaml = require('lyaml')

-- YAML string
local yaml_str = [[
name: Taro
occupation: Programmer
languages:
  - Lua
  - Python
]]

-- 解析してLuaテーブルにする
local data = lyaml.load(yaml_str)

-- dataテーブルを使った出力
print(data.name) -- 出力: Taro
for _, language in ipairs(data.languages) do
    print(language)
end
```

Sample Output:
```
Taro
Lua
Python
```

## Deep Dive (深い潜入)
YAMLは "YAML Ain't Markup Language" の略で、2001年に導入された。JSONやXMLと比べて、YAMLは人間にとって読みやすい。`lyaml`はLuaでYAMLを扱う代表的なライブラリだ。C言語で書かれた `libyaml` に基づいているため、速度も速い。

## See Also (関連情報)
- YAML公式サイト: https://yaml.org
- `lyaml` ライブラリ: https://github.com/gvvaughan/lyaml
- LuaRocksでの `lyaml` インストール: https://luarocks.org/modules/gvvaughan/lyaml
- `libyaml` GitHub リポジトリ: https://github.com/yaml/libyaml
