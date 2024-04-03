---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:52.434065-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.338599-06:00'
model: gpt-4-0125-preview
summary: "CSV\uFF08\u30AB\u30F3\u30DE\u533A\u5207\u308A\u5024\uFF09\u30D5\u30A1\u30A4\
  \u30EB\u3092\u4F7F\u7528\u3059\u308B\u4F5C\u696D\u306F\u3001\u500B\u3005\u306E\u5024\
  \u3092\u533A\u5207\u308B\u306E\u306B\u30AB\u30F3\u30DE\u3092\u4F7F\u7528\u3057\u3066\
  \u3001\u884C\u3068\u5217\u306B\u6574\u7406\u3055\u308C\u305F\u30C6\u30AD\u30B9\u30C8\
  \u30C7\u30FC\u30BF\u3092\u89E3\u6790\u304A\u3088\u3073\u751F\u6210\u3059\u308B\u3053\
  \u3068\u3092\u542B\u307F\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\
  \u3001CSV\u306E\u5E83\u7BC4\u306A\u30B5\u30DD\u30FC\u30C8\u3068\u30B7\u30F3\u30D7\
  \u30EB\u3055\u306E\u305F\u3081\u3001\u7570\u306A\u308B\u30A2\u30D7\u30EA\u30B1\u30FC\
  \u30B7\u30E7\u30F3\u3001\u30C7\u30FC\u30BF\u30D9\u30FC\u30B9\u9593\u3067\u306E\u30C7\
  \u30FC\u30BF\u4EA4\u63DB\u3092\u5BB9\u6613\u306B\u3059\u308B\u305F\u3081\u3001\u307E\
  \u305F\u306F\u30C7\u30FC\u30BF\u51E6\u7406\u3084\u5206\u6790\u30BF\u30B9\u30AF\u306E\
  \u305F\u3081\u306B\u3001\u3053\u306E\u30D7\u30ED\u30BB\u30B9\u306B\u3057\u3070\u3057\
  \u3070\u53D6\u308A\u7D44\u307F\u307E\u3059\u3002."
title: "CSV\u3068\u306E\u4F5C\u696D"
weight: 37
---

## 何を & なぜ？

CSV（カンマ区切り値）ファイルを使用する作業は、個々の値を区切るのにカンマを使用して、行と列に整理されたテキストデータを解析および生成することを含みます。プログラマーは、CSVの広範なサポートとシンプルさのため、異なるアプリケーション、データベース間でのデータ交換を容易にするため、またはデータ処理や分析タスクのために、このプロセスにしばしば取り組みます。

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
