---
title:                "Json 작업하기"
html_title:           "Ruby: Json 작업하기"
simple_title:         "Json 작업하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/working-with-json.md"
---

{{< edit_this_page >}}

# Ruby로 JSON 다루기

## 무엇이며 왜?: 

JSON 은 웹 개발에서 많이 사용되는 형식으로서 데이터를 저장하고 전송하는 데에 사용됩니다. 프로그래머들은 데이터를 보다 간결하게 처리하기 위해 JSON 을 사용합니다. 

## 하는 방법:

다음 예제는 Ruby 에서 JSON 을 다루는 방법을 보여줍니다. 

```Ruby
require 'json'

# JSON 데이터를 해시로 변환하기
json_data = '{"name": "John", "age": 30}'
hash_data = JSON.parse(json_data)
puts hash_data["name"] # "John"

# 해시를 JSON 형식으로 변환하기
hash = {name: "Emily", age: 25}
json = JSON.generate(hash)
puts json # {"name":"Emily","age":25}
```

## 깊이 들어가기:

1. JSON 의 역사: C 언어의 발명가 중 한 명이던 Douglas Crockford 가 2001년에 발표한 형식입니다. 
2. 대안: XML, YAML 과 같은 다른 데이터 형식들도 JSON 과 비슷한 용도로 사용할 수 있지만, JSON 이 가장 간결하고 빠른 처리를 제공합니다.
3. 구현 세부사항: Ruby 에서는 ```json``` 패키지를 사용하여 JSON 데이터를 처리할 수 있습니다. 

## 더 알아보기:

- [JSON 작동 원리](https://www.json.org/json-ko.html)
- [Ruby 공식 문서 - JSON 처리](https://ruby-doc.org/stdlib-2.7.1/libdoc/json/rdoc/JSON.html)