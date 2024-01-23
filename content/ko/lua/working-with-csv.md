---
title:                "CSV 파일 다루기"
html_title:           "Arduino: CSV 파일 다루기"
simple_title:         "CSV 파일 다루기"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV는 'Comma Separated Values'의 약어로, 데이터를 저장하고 교환하는 간단한 포맷입니다. 프로그래머들은 CSV를 사용하여 테이블 데이터를 편집하고 다른 언어나 프로그램 간에 쉽게 데이터를 주고받기 위해 사용합니다.

## How to:
Lua 코드로 CSV 파일 읽기와 쓰기 예제입니다:

```Lua
-- CSV 파일 읽기
local function read_csv(filePath)
    local file = io.open(filePath, "r")
    if not file then return nil end
    local data = {}
    for line in file:lines() do
        table.insert(data, line:split(','))
    end
    file:close()
    return data
end

-- CSV 파일 쓰기
local function write_csv(filePath, data)
    local file = io.open(filePath, "w")
    for _, row in ipairs(data) do
        file:write(table.concat(row, ',') .. '\n')
    end
    file:close()
end
```

실행 예시:

```Lua
local csvData = read_csv("sample.csv")
for _, row in ipairs(csvData) do
    print(table.concat(row, ', '))
end

write_csv("new_sample.csv", csvData)
```

## Deep Dive
CSV 포맷은 1972년 IBM이 선보였고, 쉬운 사용법 때문에 여전히 인기가 많습니다. 대안으로는 JSON이나 XML 같은 데이터 포맷들이 있으나, CSV는 테이블 형태의 데이터를 처리할 때 가장 직관적입니다. Lua에서 CSV를 다룰 때는 문자열 함수와 파일 I/O 기능을 사용합니다.

## See Also
- Lua 문자열 함수: https://www.lua.org/manual/5.4/manual.html#6.4
- Lua 파일 I/O: https://www.lua.org/manual/5.4/manual.html#6.8
- LuaRocks CSV 모듈: https://luarocks.org/modules/search?q=csv
