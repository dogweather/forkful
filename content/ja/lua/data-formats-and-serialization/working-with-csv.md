---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:52.434065-07:00
description: "\u65B9\u6CD5: Lua\u3067CSV\u30D5\u30A1\u30A4\u30EB\u3092\u6271\u3046\
  \u306B\u306F\u3001\u30B7\u30F3\u30D7\u30EB\u306A\u30BF\u30B9\u30AF\u306B\u5916\u90E8\
  \u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u5FC5\u8981\u3068\u305B\u305A\u306B\u3001\u8A00\
  \u8A9E\u306B\u3088\u3063\u3066\u63D0\u4F9B\u3055\u308C\u308B\u57FA\u672C\u7684\u306A\
  \u30D5\u30A1\u30A4\u30EBIO\u64CD\u4F5C\u3092\u4F7F\u7528\u3059\u308B\u30A2\u30D7\
  \u30ED\u30FC\u30C1\u304C\u3042\u308A\u307E\u3059\u3002\u7279\u5225\u306A\u30B1\u30FC\
  \u30B9\uFF08\u4F8B\u3048\u3070\u3001\u5024\u5185\u306E\u30AB\u30F3\u30DE\uFF09\u306E\
  \u51E6\u7406\u306A\u3069\u3001\u3088\u308A\u8907\u96D1\u306A\u64CD\u4F5C\u306B\u306F\
  \u3001`lua-\u2026"
lastmod: '2024-04-05T21:53:43.179590-06:00'
model: gpt-4-0125-preview
summary: "Lua\u3067CSV\u30D5\u30A1\u30A4\u30EB\u3092\u6271\u3046\u306B\u306F\u3001\
  \u30B7\u30F3\u30D7\u30EB\u306A\u30BF\u30B9\u30AF\u306B\u5916\u90E8\u30E9\u30A4\u30D6\
  \u30E9\u30EA\u3092\u5FC5\u8981\u3068\u305B\u305A\u306B\u3001\u8A00\u8A9E\u306B\u3088\
  \u3063\u3066\u63D0\u4F9B\u3055\u308C\u308B\u57FA\u672C\u7684\u306A\u30D5\u30A1\u30A4\
  \u30EBIO\u64CD\u4F5C\u3092\u4F7F\u7528\u3059\u308B\u30A2\u30D7\u30ED\u30FC\u30C1\
  \u304C\u3042\u308A\u307E\u3059\u3002\u7279\u5225\u306A\u30B1\u30FC\u30B9\uFF08\u4F8B\
  \u3048\u3070\u3001\u5024\u5185\u306E\u30AB\u30F3\u30DE\uFF09\u306E\u51E6\u7406\u306A\
  \u3069\u3001\u3088\u308A\u8907\u96D1\u306A\u64CD\u4F5C\u306B\u306F\u3001`lua-csv`\u306E\
  \u3088\u3046\u306A\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u306E\u30E9\u30A4\u30D6\
  \u30E9\u30EA\u306E\u4F7F\u7528\u304C\u6709\u76CA\u304B\u3082\u3057\u308C\u307E\u305B\
  \u3093\u3002"
title: "CSV\u3068\u306E\u4F5C\u696D"
weight: 37
---

## 方法:
LuaでCSVファイルを扱うには、シンプルなタスクに外部ライブラリを必要とせずに、言語によって提供される基本的なファイルIO操作を使用するアプローチがあります。特別なケース（例えば、値内のカンマ）の処理など、より複雑な操作には、`lua-csv`のようなサードパーティのライブラリの使用が有益かもしれません。

### CSVファイルの読み込み
こちらは、カンマ区切りに基づいて各行を値に分割してCSVファイルを行ごとに読むシンプルな例です。

```lua
function parseCSVLine(line)
    local result = {}
    local from = 1
    local sep = ","
    local field
    while true do
        local start, finish = string.find(line, sep, from)
        if not start then
            table.insert(result, string.sub(line, from))
            break
        end
        field = string.sub(line, from, start - 1)
        table.insert(result, field)
        from = finish + 1
    end
    return result
end

local file = io.open("example.csv", "r")
for line in file:lines() do
    local values = parseCSVLine(line)
    for i, v in ipairs(values) do
        print(i, v)
    end
end
file:close()
```

**サンプル出力**（"name,age\newlineJohn Doe,30\newlineJane Doe,32"という内容の`example.csv`に対して）:
```
1	name
2	age
1	John Doe
2	30
1	Jane Doe
2	32
```

### CSVファイルの作成
CSVファイルを生成するには、カンマ区切りの値で文字列を構築して、行ごとにファイルに書き込むだけです。

```lua
local data = {
    {"name", "age"},
    {"John Doe", "30"},
    {"Jane Doe", "32"}
}

local file = io.open("output.csv", "w")
for _, v in ipairs(data) do
    file:write(table.concat(v, ","), "\n")
end
file:close()
```

これにより、指定されたデータを持つ`output.csv`ファイルが作成（または上書き）されます。

### lua-csvを使用する
引用符やエスケープ文字のサポートを含む、より高度なCSV処理のためには、`lua-csv`ライブラリが堅牢な選択となります。

まず、LuaRocksを使用してインストールします:
```shell
luarocks install lua-csv
```

その後、CSVファイルを読むことは以下のように簡単になります:

```lua
local csv = require("csv")

-- ファイルから読む
for fields in csv.open("example.csv") do
    for i, v in ipairs(fields) do
        print(i, v)
    end
end
```

適切な引用符とエスケープでCSVに書き込むには:

```lua
local file = csv.open("output.csv", {write=true})

local data = {
    {"name", "profession", "location"},
    {"John Doe", "Software Engineer", "New York, NY"},
    {"Jane Doe", "Data Scientist", "\"San Francisco, CA\""}
}

for _, v in ipairs(data) do
    file:write(v)
end
```

このアプローチは、値内のカンマや引用符などの複雑さを自動的に処理します。
