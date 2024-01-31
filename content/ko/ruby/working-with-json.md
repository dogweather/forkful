---
title:                "JSON 다루기"
date:                  2024-01-19
html_title:           "Arduino: JSON 다루기"
simple_title:         "JSON 다루기"

category:             "Ruby"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
JSON은 데이터 교환 포맷입니다. 프로그래머들은 구조화된 데이터를 저장하고 통신하기 위해 JSON을 사용합니다.

## How to: (어떻게 하나요?)
```Ruby
require 'json'

# JSON 생성하기
user = { name: "Kim", age: 30, city: "Seoul" }
user_json = user.to_json
puts user_json # => {"name":"Kim","age":30,"city":"Seoul"}

# JSON 파싱하기
parsed_user = JSON.parse(user_json)
puts parsed_user["name"] # => Kim
```

## Deep Dive (심층 분석)
JSON은 JavaScript Object Notation의 약자이고, 처음에는 JavaScript에서 데이터를 객체로 나타내기 위해 만들어졌습니다. XML과 같은 다른 데이터 포맷에 비해 더 읽기 쉽고 가벼운게 특징입니다. Ruby에서 `json` 모듈을 사용하면 간편하게 JSON을 생성하고 파싱할 수 있습니다. C언어로 작성된 확장 모듈이기 때문에 성능도 뛰어납니다.

## See Also (참고 자료)
- JSON 표준: [JSON](http://www.json.org/json-en.html)
- JSON과 XML 비교: [JSON vs XML](https://www.w3schools.com/js/js_json_xml.asp)
