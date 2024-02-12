---
title:                "JSON과 함께 일하기"
aliases: - /ko/ruby/working-with-json.md
date:                  2024-02-03T19:24:34.364456-07:00
model:                 gpt-4-0125-preview
simple_title:         "JSON과 함께 일하기"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇이고 왜 사용할까?

JSON(JavaScript Object Notation)은 클라이언트와 서버 간의 데이터 교환을 위해 웹 애플리케이션에서 널리 사용되는 가벼운 데이터 교환 형식입니다. 프로그래머는 외부 소스로부터 받은 데이터를 파싱하거나 API 응답을 위한 데이터 포맷으로 JSON을 사용하여, 이해하기 쉬운 구조를 활용해 데이터 조작 및 저장을 쉽게 할 수 있습니다.

## 어떻게 사용할까:

Ruby는 표준 라이브러리를 통해 JSON을 파싱하고 생성하는 데 있어 매끄러운 방법을 제공합니다. 이러한 작업을 위한 주요 모듈은 `json`으로, Ruby 애플리케이션에 쉽게 통합될 수 있습니다.

### JSON 파싱:

JSON 문자열을 Ruby 해시로 변환하려면 `JSON.parse` 메소드를 사용할 수 있습니다.

```ruby
require 'json'

json_string = '{"name": "John Doe", "age": 30, "city": "New York"}'
ruby_hash = JSON.parse(json_string)

puts ruby_hash
# 출력: {"name"=>"John Doe", "age"=>30, "city"=>"New York"}
```

### JSON 생성:

반대로, Ruby 해시를 JSON 문자열로 변환하기 위해서는 `JSON.generate` 메소드나 `json` 라이브러리가 필요한 경우 Ruby 객체에서 사용할 수 있는 `to_json` 메소드를 사용합니다.

```ruby
require 'json'

ruby_hash = { name: "Jane Doe", age: 25, city: "Los Angeles" }
json_string = ruby_hash.to_json

puts json_string
# 출력: {"name":"Jane Doe","age":25,"city":"Los Angeles"}
```

### 서드파티 라이브러리:

Ruby의 표준 라이브러리가 기본적인 JSON 처리를 다루지만, 많은 프로젝트에서는 기능 향상 및 성능을 위해 서드파티 라이브러리에 의존합니다. 인기 있는 선택 중 하나는 `Oj` (Optimized JSON)입니다.

#### Oj로 파싱하기:

```ruby
require 'oj'

json_string = '{"name": "Alex", "age": 40, "city": "Chicago"}'
ruby_hash = Oj.load(json_string)

puts ruby_hash
# 출력: {"name"=>"Alex", "age"=>40, "city"=>"Chicago"}
```

#### Oj로 생성하기:

Oj는 또한 Ruby 객체로부터 JSON을 빠르게 생성하는 방법을 제공합니다:

```ruby
require 'oj'

ruby_hash = { name: "Samantha", age: 35, city: "Miami" }
json_string = Oj.dump(ruby_hash)

puts json_string
# 출력: {"name":"Samantha","age":35,"city":"Miami"}
```

이 예시들은 Ruby에서 JSON을 다루는 것이 단순한 데이터 조작부터 복잡한 API 통신에 이르기까지 다양한 작업에 접근하기 쉽게 만든다는 것을 보여줍니다.
