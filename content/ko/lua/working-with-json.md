---
title:                "json으로 작업하기"
html_title:           "Lua: json으로 작업하기"
simple_title:         "json으로 작업하기"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/working-with-json.md"
---

{{< edit_this_page >}}

# 무엇 & 왜?

JSON(JavaScript Object Notation)은 데이터를 효과적으로 저장하고 교환하기 위해 개발된 가벼운 형식입니다. 프로그래머들은 JSON 형식의 데이터를 사용하여 서로 다른 시스템 간에 통신이나 저장을 쉽게 할 수 있습니다.

# 방법:

JSON 데이터를 Lua에서 다루는 방법은 매우 간단합니다. `json` 라이브러리를 사용하여 JSON 데이터를 읽고, 쓰고, 변환할 수 있습니다. 아래는 간단한 사용 예시입니다.

```Lua
local json = require("json")

-- JSON 데이터 읽기
local data = [[{
  "name": "John",
  "age": 25,
  "hobbies": ["reading", "coding", "playing"]
}]]
local decodedData = json.parse(data)
print(decodedData.name) --> "John"
print(decodedData.hobbies[1]) --> "reading"

-- 새로운 JSON 데이터 생성
local newData = {
  name = "Jane",
  age = 30,
  hobbies = {"drawing", "cooking", "playing"}
}
local encodedData = json.stringify(newData)
print(encodedData) --> {"name":"Jane","age":30,"hobbies":["drawing","cooking","playing"]}
``` 

# 깊은 곳 살펴보기:

JSON은 1990년대 초반에 더글라스 크락포드가 만든 형식으로서, 초기 웹 개발에서 많이 사용되었습니다. JSON은 XML과 비교하여 훨씬 간결하고 가볍기 때문에 대안으로 널리 사용되었습니다. 이제는 대부분의 웹 서비스와 API에서 JSON을 기본 데이터 형식으로 사용하고 있습니다. 

Lua에서는 `cjson` 라이브러리와 같은 다른 JSON 라이브러리를 사용할 수도 있습니다. 하지만 대부분의 경우 `json` 라이브러리가 가장 간단하고 직관적인 방법입니다.

# 관련 자료:

- [JSON 공식 사이트](https://www.json.org/json-ko.html)
- [Lua에서 JSON 다루는 방법](https://shane-xdepth.netlify.app/programming/json-in-lua/)