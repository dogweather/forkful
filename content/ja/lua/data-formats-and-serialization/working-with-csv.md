---
title:                "CSVとの作業"
aliases:
- /ja/lua/working-with-csv.md
date:                  2024-02-03T19:20:52.434065-07:00
model:                 gpt-4-0125-preview
simple_title:         "CSVとの作業"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
