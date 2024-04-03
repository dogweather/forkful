---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:35.092992-07:00
description: "Lua\uC5D0\uC11C JSON\uC744 \uB2E4\uB8E8\uB294 \uAC83\uC740 JSON \uD615\
  \uC2DD\uC758 \uBB38\uC790\uC5F4\uC744 Lua \uD14C\uC774\uBE14\uB85C \uD30C\uC2F1\uD558\
  \uACE0 \uADF8 \uBC18\uB300\uC758 \uC791\uC5C5\uC744 \uC218\uD589\uD568\uC744 \uD3EC\
  \uD568\uD558\uC5EC, Lua \uC560\uD50C\uB9AC\uCF00\uC774\uC158\uACFC \uC6F9 \uC11C\
  \uBE44\uC2A4 \uB610\uB294 \uC678\uBD80 API \uAC04\uC758 \uC26C\uC6B4 \uB370\uC774\
  \uD130 \uAD50\uD658\uC774 \uAC00\uB2A5\uD558\uAC8C \uD569\uB2C8\uB2E4. \uD504\uB85C\
  \uADF8\uB798\uBA38\uB4E4\uC740 JSON\uC758 \uAC00\uBCBC\uC6C0\uACFC \uC27D\uAC8C\
  \ \uD30C\uC2F1\uD560 \uC218 \uC788\uB294 \uD615\uC2DD\uC744\u2026"
lastmod: '2024-03-13T22:44:55.448737-06:00'
model: gpt-4-0125-preview
summary: "Lua\uC5D0\uC11C JSON\uC744 \uB2E4\uB8E8\uB294 \uAC83\uC740 JSON \uD615\uC2DD\
  \uC758 \uBB38\uC790\uC5F4\uC744 Lua \uD14C\uC774\uBE14\uB85C \uD30C\uC2F1\uD558\uACE0\
  \ \uADF8 \uBC18\uB300\uC758 \uC791\uC5C5\uC744 \uC218\uD589\uD568\uC744 \uD3EC\uD568\
  \uD558\uC5EC, Lua \uC560\uD50C\uB9AC\uCF00\uC774\uC158\uACFC \uC6F9 \uC11C\uBE44\
  \uC2A4 \uB610\uB294 \uC678\uBD80 API \uAC04\uC758 \uC26C\uC6B4 \uB370\uC774\uD130\
  \ \uAD50\uD658\uC774 \uAC00\uB2A5\uD558\uAC8C \uD569\uB2C8\uB2E4."
title: "JSON\uACFC \uD568\uAED8 \uC77C\uD558\uAE30"
weight: 38
---

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
