---
title:                "JSON으로 작업하기"
html_title:           "Bash: JSON으로 작업하기"
simple_title:         "JSON으로 작업하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/working-with-json.md"
---

{{< edit_this_page >}}

## 왜
JSON 작업을 하는 이유는 데이터를 저장하고 교환하기 위해서이며, 이는 다양한 시스템 및 프로그래밍 언어에서 널리 사용됩니다.

## 사용 방법
JSON 파싱 및 생성을 위해 Bash에서 다음과 같은 명령어를 사용할 수 있습니다.
```
#!/bin/bash

# JSON 파싱 예시
echo '{"name": "John", "age": 30, "city": "Seoul"}' | jq '.age'

# 출력: 30

# JSON 생성 예시
echo '{"name": "Jane", "age": 25}' | jq '.country = "South Korea"'

# 출력: {
#     "name": "Jane",
#     "age": 25,
#     "country": "South Korea" 
#}
```

## 깊이 파헤치기
JSON은 "JavaScript Object Notation"의 약자로, 웹 개발에서 자주 사용됩니다. Bash에서도 jq라는 툴을 사용하여 쉽게 파싱하고 생성할 수 있습니다. 또한 JSON을 활용하여 데이터를 다른 시스템으로 전송하는 등 다양한 용도로 활용할 수 있습니다.

## 참고
- [Bash에서 jq 사용하기 (영어)](https://stedolan.github.io/jq/)
- [JSON 공식 사이트 (영어)](https://www.json.org/json-en.html)
- [Bash 강좌 시리즈 (한국어)](https://academy.nomadcoders.co/courses/enrolled/435558)