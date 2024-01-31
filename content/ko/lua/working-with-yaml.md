---
title:                "YAML 다루기"
date:                  2024-01-19
html_title:           "Arduino: YAML 다루기"
simple_title:         "YAML 다루기"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML은 데이터를 표현하는 언어입니다. 프로그래머들은 설정 파일, 데이터 저장, 메시지 교환이나 서버와 클라이언트 간의 통신을 위해 YAML을 많이 사용합니다.

## How to:
Lua에서 YAML을 다루려면 `lyaml` 라이브러리를 사용할 수 있습니다. 아래는 간단한 예시 코드와 결과입니다.

```Lua
-- lyaml 라이브러리 설치 필요
local lyaml = require('lyaml')

-- YAML 문자열을 Lua 테이블로 파싱
local yaml_data = [[
- name: Kim
  age: 25
- name: Lee
  age: 28
]]
local people = lyaml.load(yaml_data)
print(people[1].name)  -- 결과: Kim

-- Lua 테이블을 YAML 문자열로 변환
local people_table = {
  { name = "Park", age = 33 },
  { name = "Choi", age = 41 }
}
local yaml_string = lyaml.dump(people_table)
print(yaml_string)
```

## Deep Dive
YAML은 "YAML Ain't Markup Language"의 약어이며 데이터 직렬화 포맷으로 사용됩니다. 2001년에 등장했고, JSON, XML과 같은 다른 데이터 표현 언어들에 비해 읽기 쉽고 쓰기 쉬운 것이 특징입니다. YAML은 구성 관리, 문서 교환 등에서 널리 채택되고 있으며 Lua에서는 lyaml 이나 다른 서드 파티 라이브러리를 통해 YAML 데이터를 처리할 수 있습니다.

## See Also
- `lyaml` 라이브러리: http://github.com/gvvaughan/lyaml
- YAML 공식 사이트: https://yaml.org
- YAML 문법 가이드: https://learnxinyminutes.com/docs/yaml/
