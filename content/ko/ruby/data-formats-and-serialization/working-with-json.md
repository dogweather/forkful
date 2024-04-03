---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:34.364456-07:00
description: "JSON(JavaScript Object Notation)\uC740 \uD074\uB77C\uC774\uC5B8\uD2B8\
  \uC640 \uC11C\uBC84 \uAC04\uC758 \uB370\uC774\uD130 \uAD50\uD658\uC744 \uC704\uD574\
  \ \uC6F9 \uC560\uD50C\uB9AC\uCF00\uC774\uC158\uC5D0\uC11C \uB110\uB9AC \uC0AC\uC6A9\
  \uB418\uB294 \uAC00\uBCBC\uC6B4 \uB370\uC774\uD130 \uAD50\uD658 \uD615\uC2DD\uC785\
  \uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB294 \uC678\uBD80 \uC18C\uC2A4\uB85C\
  \uBD80\uD130 \uBC1B\uC740 \uB370\uC774\uD130\uB97C \uD30C\uC2F1\uD558\uAC70\uB098\
  \ API \uC751\uB2F5\uC744 \uC704\uD55C \uB370\uC774\uD130 \uD3EC\uB9F7\uC73C\uB85C\
  \ JSON\uC744\u2026"
lastmod: '2024-03-13T22:44:56.027082-06:00'
model: gpt-4-0125-preview
summary: "JSON(JavaScript Object Notation)\uC740 \uD074\uB77C\uC774\uC5B8\uD2B8\uC640\
  \ \uC11C\uBC84 \uAC04\uC758 \uB370\uC774\uD130 \uAD50\uD658\uC744 \uC704\uD574 \uC6F9\
  \ \uC560\uD50C\uB9AC\uCF00\uC774\uC158\uC5D0\uC11C \uB110\uB9AC \uC0AC\uC6A9\uB418\
  \uB294 \uAC00\uBCBC\uC6B4 \uB370\uC774\uD130 \uAD50\uD658 \uD615\uC2DD\uC785\uB2C8\
  \uB2E4."
title: "JSON\uACFC \uD568\uAED8 \uC77C\uD558\uAE30"
weight: 38
---

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
