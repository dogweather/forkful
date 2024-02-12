---
title:                "CSV와 함께 작업하기"
date:                  2024-02-03T19:20:44.125056-07:00
model:                 gpt-4-0125-preview
simple_title:         "CSV와 함께 작업하기"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

CSV(쉼표로 구분된 값) 파일을 다루는 작업은 개별 값들을 쉼표로 구분하여 행과 열로 조직된 텍스트 데이터를 파싱하고 생성하는 것을 포함합니다. 프로그래머들은 종종 다양한 애플리케이션, 데이터베이스 간의 데이터 교환을 용이하게 하거나 데이터 처리 및 분석 작업을 위해 이 과정에 참여하며, 이는 CSV의 광범위한 지원과 단순성 때문입니다.

## 방법:

Lua에서는 CSV 파일을 다루는 작업을 외부 라이브러리 없이도 언어가 제공하는 기본 파일 IO 작업을 사용하여 간단한 작업을 위해 접근할 수 있습니다. 더 복잡한 작업에 대해서는, 예를 들어 값 내에 쉼표가 있는 경우와 같은 특별한 경우를 처리하는 것처럼, `lua-csv`와 같은 타사 라이브러리를 사용하는 것이 유익할 수 있습니다.

### CSV 파일 읽기
다음은 쉼표 구분자를 기반으로 각 줄을 값으로 분리하여 CSV 파일을 한 줄씩 읽는 간단한 예제입니다.

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

**샘플 출력** ("name,age\newlineJohn Doe,30\newlineJane Doe,32" 내용을 가진 `example.csv`의 경우):
```
1	name
2	age
1	John Doe
2	30
1	Jane Doe
2	32
```

### CSV 파일 작성하기
CSV 파일을 생성하기 위해서는 쉼표로 구분된 값을 갖는 문자열을 구성하여 파일에 한 줄씩 쓰기만 하면 됩니다.

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

이것은 지정된 데이터로 `output.csv` 파일을 생성(또는 덮어쓰기)합니다.

### lua-csv 사용하기
따옴표와 이스케이프 문자를 지원하는 더 고급 CSV 처리를 위해서는 `lua-csv` 라이브러리가 견고한 선택입니다.

먼저 LuaRocks를 사용하여 설치하세요:
```shell
luarocks install lua-csv
```

그런 다음, CSV 파일을 읽기는 다음과 같이 간단해집니다:

```lua
local csv = require("csv")

-- 파일에서 읽기
for fields in csv.open("example.csv") do
    for i, v in ipairs(fields) do
        print(i, v)
    end
end
```

그리고 제대로 된 따옴표와 이스케이프로 CSV에 쓰기:

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

이 방법은 값 내의 쉼표와 따옴표와 같은 복잡성을 자동으로 처리합니다.
