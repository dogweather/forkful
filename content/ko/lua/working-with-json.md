---
title:                "JSON 다루기"
html_title:           "Arduino: JSON 다루기"
simple_title:         "JSON 다루기"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
JSON은 데이터 교환 형식입니다. 프로그래머들은 설정, API 통신, 서버와 클라이언트 간 정보 교환에 JSON을 사용합니다.

## How to: (어떻게 하나요?)
Lua에서 JSON을 다루기 위해, `dkjson` 모듈을 사용하겠습니다. 설치 후, JSON을 파싱하고 문자열로 변환하는 기본 예제를 보세요.

```Lua
local dkjson = require 'dkjson'

-- JSON 문자열 파싱
local jsonString = '{"name": "홍길동", "age": 25, "isProgrammer": true}'
local userData, pos, err = dkjson.decode(jsonString, 1, nil)
if err then
  print ("Error:", err)
else
  print(userData.name) -- 홍길동
end

-- 테이블을 JSON 문자열로
local petData = {
  type = "고양이",
  age = 3
}
local jsonPet, err = dkjson.encode(petData, { indent = true })
if err then
  print("Error:", err)
else
  print(jsonPet)
end
```
결과:
```
홍길동
{
  "age": 3,
  "type": "고양이"
}
```

## Deep Dive (깊이 있는 정보)
JSON(JavaScript Object Notation)은 2001년 Douglas Crockford에 의해 발표되었으며, 텍스트 기반의 가벼운 데이터 교환 양식입니다. Lua에서 JSON을 다루는 라이브러리로는 `dkjson`, `cjson`, `json.lua`가 있습니다. `dkjson`은 순수 Lua 코드로 구성되어 플랫폼 독립적이지만, `cjson`은 C로 작성되어 속도가 빠릅니다.

## See Also (더 보기)
- `dkjson` 라이브러리: http://dkolf.de/src/dkjson-lua.fsl/home
- Lua 공식 문서: https://www.lua.org/docs.html
- JSON 공식 웹사이트: https://www.json.org/json-en.html
- JSON과 Lua: http://lua-users.org/wiki/JsonModules