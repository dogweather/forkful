---
title:                "Ruby: json 작업하기"
simple_title:         "json 작업하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/working-with-json.md"
---

{{< edit_this_page >}}

## 왜

JSON을 다루는 작업을 하는 이유는 여러 가지가 있습니다. 대부분의 웹 애플리케이션에서는 데이터를 교환하기 위해 JSON을 사용하며, Ruby로 작성된 애플리케이션의 경우 JSON을 처리하는 방법을 알고 있어야 합니다. 또한 다른 언어로 작성된 애플리케이션과 통신할 때도 JSON을 사용할 수 있습니다.

## 어떻게

우선, RubyGems에서 'json' 라이브러리를 설치해야 합니다. 그런 다음, 다음과 같이 코드를 작성합니다.

```Ruby
# JSON 라이브러리 불러오기
require 'json'

# JSON 형식의 데이터 작성
data = { "name" => "홍길동", "age" => 30 }

# JSON 형식으로 변환하기
json_data = JSON.generate(data)

# 생성된 JSON 데이터 출력하기
puts json_data
```

위 코드를 실행하면 다음과 같이 출력됩니다.

```Ruby
{"name":"홍길동","age":30}
```

JSON 데이터를 다루기 위해 필수적으로 알아야 하는 것 중 하나는 JSON 형식의 문자열을 해석하는 것입니다. 이를 위해 'parse' 메소드를 사용합니다. 아래 예제를 보시죠.

```Ruby
# JSON 데이터
json_data = '{"name":"홍길동","age":30}'

# 문자열을 해석하여 Ruby의 데이터 타입으로 변경하기
data = JSON.parse(json_data)

# 문자열을 해석한 데이터 사용하기
puts "이름: #{data["name"]}, 나이: #{data["age"]}세"
```

실행하면 다음과 같이 출력됩니다.

```Ruby
이름: 홍길동, 나이: 30세
```

## 딥 다이브

JSON은 자바스크립트에서 사용하는 데이터 포맷인데, 그럼에도 불구하고 Ruby에서도 완벽하게 처리할 수 있습니다. Ruby의 Hash와 Array를 이용하여 JSON의 객체와 배열을 다룰 수 있습니다. 또한 'generate' 메소드를 사용하여 Ruby의 데이터를 JSON 형식으로 바꿀 수 있습니다. JSON을 정확히 파악하고 Ruby에서 다양한 형태로 다룰 수 있다면, 웹 애플리케이션을 개발하는 데 큰 도움이 될 것입니다.

## 더 알아보기

* [JSON 공식 사이트](https://www.json.org/json-ko.html)
* [RubyGems에서 'json' 라이브러리 확인하기](https://rubygems.org/gems/json)
* [Ruby에서 JSON 다루기](https://ruby-doc.org/stdlib-2.7.2/libdoc/json/rdoc/JSON.html)