---
title:                "CSVファイルの操作"
date:                  2024-01-19
html_title:           "Arduino: CSVファイルの操作"
simple_title:         "CSVファイルの操作"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? 何となぜ？
CSVは「Comma-Separated Values」の略だ。テキストデータを簡単に表形式で保持、交換するために使う。プログラマはデータを保存したり他のプログラムと共有するためにCSVを扱う。

## How to: どうやるか
LuaでCSVファイルを読み込む、編集する、そして書き出す基礎を見ていこう。

```Lua
-- CSVファイルを読み込む
function read_csv(file_path)
  local file = io.open(file_path, "r")
  local data = {}
  for line in file:lines() do
    table.insert(data, line:split(","))
  end
  file:close()
  return data
end

-- ストリングのスプリット機能を追加する（Luaには標準でないので）
function string:split(separator)
  local fields = {}
  local pattern = string.format("([^%s]+)", separator)
  self:gsub(pattern, function(c) fields[#fields+1] = c end)
  return fields
end

-- CSVを編集する
function edit_csv(data)
  -- たとえば、全ての行の最初の値を大文字にする
  for _, row in ipairs(data) do
    row[1] = row[1]:upper()
  end
end

-- CSVファイルに書き出す
function write_csv(file_path, data)
  local file = io.open(file_path, "w")
  for _, row in ipairs(data) do
    file:write(table.concat(row, ",") .. "\n")
  end
  file:close()
end

-- 実行例
local csv_data = read_csv("example.csv")
edit_csv(csv_data)
write_csv("example_modified.csv", csv_data)
```

## Deep Dive 深掘り
CSVは1983年にIBM PCで使われ始めた。JSONやXMLなどの形式があるが、CSVはそのシンプルさで広く使われる。CSVファイルはテキストエディタで開け、人間が読める。しかし、CSVは規格が厳格でなく、パースが面倒な場合もある。Luaでは、標準ライブラリが限られているため、CSVを操作するには独自の関数を書くか、外部ライブラリを使用する必要がある。

## See Also 関連情報
- Lua公式ドキュメント: https://www.lua.org/manual/5.4/
- CSVの仕様（RFC 4180）: https://tools.ietf.org/html/rfc4180
- LuaRocksで利用可能なCSVライブラリ: https://luarocks.org/modules/tags/csv
