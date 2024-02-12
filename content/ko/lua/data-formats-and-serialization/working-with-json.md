---
title:                "JSON과 함께 일하기"
date:                  2024-02-03T19:23:35.092992-07:00
model:                 gpt-4-0125-preview
simple_title:         "JSON과 함께 일하기"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?
Lua에서 JSON을 다루는 것은 JSON 형식의 문자열을 Lua 테이블로 파싱하고 그 반대의 작업을 수행함을 포함하여, Lua 애플리케이션과 웹 서비스 또는 외부 API 간의 쉬운 데이터 교환이 가능하게 합니다. 프로그래머들은 JSON의 가벼움과 쉽게 파싱할 수 있는 형식을 이용하여 데이터 저장, 설정 또는 API 통신을 효율적으로 수행하기 위해 이 작업을 합니다.

## 어떻게:
Lua는 JSON 처리를 위한 내장 라이브러리를 포함하고 있지 않습니다. 따라서, `dkjson`과 같은 인기 있는 타사 라이브러리 중 하나를 JSON 인코딩 및 디코딩에 쉽게 사용할 수 있습니다. 먼저, 예를 들어 LuaRocks(`luarocks install dkjson`)를 통해 `dkjson`을 설치하세요, 그리고 아래 예시를 따라해 보세요.

### JSON을 Lua 테이블로 디코딩하기
```lua
local dkjson = require "dkjson"

local jsonString = '{"name": "Lua 프로그래머", "age": 30, "languages": ["Lua", "JavaScript"]}'
local luaTable, pos, err = dkjson.decode(jsonString, 1, nil)
if err then
  print ("오류:", err)
else
  print("이름:", luaTable.name) -- 출력: 이름: Lua 프로그래머
  print("나이:", luaTable.age) -- 출력: 나이: 30
  print("언어:", table.concat(luaTable.languages, ", ")) -- 출력: 언어: Lua, JavaScript
end
```

### Lua 테이블을 JSON으로 인코딩하기
```lua
local dkjson = require "dkjson"

local luaTable = {
  name = "Lua 프로그래머",
  age = 30,
  languages = { "Lua", "JavaScript" }
}

local jsonString = dkjson.encode(luaTable, { indent = true })
print(jsonString)
```

인코딩을 위한 샘플 출력:
```json
{
  "age": 30,
  "languages": [
    "Lua",
    "JavaScript"
  ],
  "name": "Lua 프로그래머"
}
```

이 간단한 예시들은 Lua에서 JSON을 다루는 방법을 보여주며, 다양한 웹 기술 및 외부 API와 Lua 애플리케이션을 쉽게 통합할 수 있게 합니다. `dkjson`이 이 예시에서 사용되었지만, 프로젝트의 요구에 따라 `cjson`이나 `RapidJSON`과 같은 다른 라이브러리도 적절한 대안이 될 수 있다는 것을 기억하세요.
