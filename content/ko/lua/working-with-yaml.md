---
title:                "Yaml 작업하기"
html_title:           "Lua: Yaml 작업하기"
simple_title:         "Yaml 작업하기"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/working-with-yaml.md"
---

{{< edit_this_page >}}

"## 무엇 & 왜?"
YAML을 다루는 것은 프로그래머에게 있어 중요한 일입니다. 쉽게 말해, YAML은 데이터를 모델링하고 저장하기 위한 형식이에요. 프로그래머들은 이 형식을 사용하여 복잡한 데이터를 쉽게 다룰 수 있기 때문에 YAML을 사용합니다.

"## 방법:"
```Lua
-- YAML 라이브러리 불러오기
local yaml = require "yaml"

-- 텍스트 형식의 YAML 데이터 선언하기
local data = [[
- name: John
  age: 27
  occupation: Programmer
]]

-- 데이터 로드하기
local loadedData = yaml.load(data)

-- 데이터 출력하기
print(loadedData[1].name) -- 결과: "John"
print(loadedData[1].age) -- 결과: 27
print(loadedData[1].occupation) -- 결과: "Programmer"
```

"## 깊은 곳 탐방:"
YAML은 2001년에 개발된 데이터 직렬화 형식입니다. XML이나 JSON과 비슷한 목적으로 사용되지만, YAML은 인간이 읽고 쓰기 쉽도록 설계되었습니다. 다른 대안으로는 TOML과 YAML의 조합인 YML이 있습니다. Lua에서는 yaml 라이브러리를 이용하여 YAML 데이터를 다룰 수 있습니다.

"## 관련 자료:"
- 공식 YAML 사이트: https://yaml.org/
- YAML 소개: https://www.datacamp.com/community/tutorials/yaml-python
- Lua에서 YAML 사용 예제: https://github.com/Neopallium/lua-yaml