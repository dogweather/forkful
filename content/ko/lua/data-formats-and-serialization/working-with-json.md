---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:35.092992-07:00
description: "\uC5B4\uB5BB\uAC8C: Lua\uB294 JSON \uCC98\uB9AC\uB97C \uC704\uD55C \uB0B4\
  \uC7A5 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uD3EC\uD568\uD558\uACE0 \uC788\uC9C0\
  \ \uC54A\uC2B5\uB2C8\uB2E4. \uB530\uB77C\uC11C, `dkjson`\uACFC \uAC19\uC740 \uC778\
  \uAE30 \uC788\uB294 \uD0C0\uC0AC \uB77C\uC774\uBE0C\uB7EC\uB9AC \uC911 \uD558\uB098\
  \uB97C JSON \uC778\uCF54\uB529 \uBC0F \uB514\uCF54\uB529\uC5D0 \uC27D\uAC8C \uC0AC\
  \uC6A9\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uBA3C\uC800, \uC608\uB97C \uB4E4\uC5B4\
  \ LuaRocks(`luarocks install\u2026"
lastmod: '2024-03-13T22:44:55.448737-06:00'
model: gpt-4-0125-preview
summary: "Lua\uB294 JSON \uCC98\uB9AC\uB97C \uC704\uD55C \uB0B4\uC7A5 \uB77C\uC774\
  \uBE0C\uB7EC\uB9AC\uB97C \uD3EC\uD568\uD558\uACE0 \uC788\uC9C0 \uC54A\uC2B5\uB2C8\
  \uB2E4."
title: "JSON\uACFC \uD568\uAED8 \uC77C\uD558\uAE30"
weight: 38
---

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
