---
aliases:
- /ko/lua/working-with-toml/
date: 2024-01-26 04:24:37.755464-07:00
description: "TOML\uC744 \uC0AC\uC6A9\uD558\uB294 \uAC83\uC740 Lua\uB85C TOML(Tom's\
  \ Obvious, Minimal Language) \uB370\uC774\uD130\uB97C \uD30C\uC2F1\uD558\uACE0 \uC0DD\
  \uC131\uD558\uB294 \uAC83\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\
  \uB798\uBA38\uB4E4\uC740 \uB370\uC774\uD130 \uAD6C\uC870\uB85C \uC27D\uAC8C \uBCC0\
  \uD658\uB418\uB294 \uC77D\uAE30 \uC27D\uACE0 \uAC04\uB2E8\uD55C \uBB38\uBC95 \uB54C\
  \uBB38\uC5D0 \uAD6C\uC131 \uD30C\uC77C\uC5D0 TOML\uC744 \uC0AC\uC6A9\uD569\uB2C8\
  \uB2E4."
lastmod: 2024-02-18 23:09:06.453065
model: gpt-4-0125-preview
summary: "TOML\uC744 \uC0AC\uC6A9\uD558\uB294 \uAC83\uC740 Lua\uB85C TOML(Tom's Obvious,\
  \ Minimal Language) \uB370\uC774\uD130\uB97C \uD30C\uC2F1\uD558\uACE0 \uC0DD\uC131\
  \uD558\uB294 \uAC83\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\
  \uBA38\uB4E4\uC740 \uB370\uC774\uD130 \uAD6C\uC870\uB85C \uC27D\uAC8C \uBCC0\uD658\
  \uB418\uB294 \uC77D\uAE30 \uC27D\uACE0 \uAC04\uB2E8\uD55C \uBB38\uBC95 \uB54C\uBB38\
  \uC5D0 \uAD6C\uC131 \uD30C\uC77C\uC5D0 TOML\uC744 \uC0AC\uC6A9\uD569\uB2C8\uB2E4\
  ."
title: "\uD504\uB85C\uADF8\uB798\uBA38\uB97C \uC704\uD55C TOML \uB2E4\uB8E8\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
TOML을 사용하는 것은 Lua로 TOML(Tom's Obvious, Minimal Language) 데이터를 파싱하고 생성하는 것을 포함합니다. 프로그래머들은 데이터 구조로 쉽게 변환되는 읽기 쉽고 간단한 문법 때문에 구성 파일에 TOML을 사용합니다.

## 방법:
먼저, Lua 환경에 TOML 파서가 있는지 확인하세요. 이 예시에서는 `lua-toml`을 사용하겠습니다.

```Lua
local toml = require("toml")

-- TOML 문자열 파싱
local toml_data = [[
title = "TOML 예제"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
]]

local data = toml.parse(toml_data)
print(data.title) -- "TOML 예제"

-- TOML 문자열 생성
local table_data = {
  title = "TOML 예제",
  owner = {
    name = "Tom Preston-Werner",
    dob = os.time({year=1979, month=5, day=27, hour=7, min=32})
  }
}

local toml_string = toml.encode(table_data)
print(toml_string)
```

예시 출력:
```
TOML 예제
```

## 심층 탐구
TOML은 2013년 Tom Preston-Werner에 의해 XML과 YAML과 같은 다른 데이터 직렬화 언어에 대한 대안으로 생성되었으며, 구성 데이터를 나타내기 위한 더 직관적인 형식을 제공합니다. JSON이 널리 사용되지만, 구성 파일에 있어서는 문법이 번거로울 수 있습니다. TOML은 .ini 파일을 닮았지만 중첩 기능과 데이터 유형이 있는 사람에게 더 명확한 문법으로 빛납니다.

TOML의 대안으로는 JSON, YAML, XML이 있습니다. 그러나 TOML은 구성을 위해 특별히 설계되었으며 YAML보다 단순하고, 구성 목적에 있어서 JSON보다 읽기 쉬우며, XML보다 덜 장황합니다.

Lua에서 TOML 처리를 구현하는 것은 일반적으로 제3자 라이브러리가 필요합니다. 기본 파싱에서부터 전체 직렬화 지원에 이르기까지 성능과 기능은 다양할 수 있습니다. 큰 구성 파일을 다루거나 자주 읽기/쓰기 작업을 수행할 때는, 라이브러리의 성능과 최신 TOML 버전과의 호환성을 고려하세요.

## 참고자료
- TOML 명세: https://toml.io/en/
- `lua-toml` 라이브러리: https://github.com/jonstoler/lua-toml
- 데이터 직렬화 형식 비교: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
