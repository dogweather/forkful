---
title:                "JSON 작업하기"
html_title:           "Ruby: JSON 작업하기"
simple_title:         "JSON 작업하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/working-with-json.md"
---

{{< edit_this_page >}}

## 왜

왜 누군가는 JSON 작업에 참여하게 될까요? JSON(JavaScript Object Notation)은 데이터를 저장하고 전송하는 데에 매우 유용한 간단한 파일 형식입니다. Ruby는 JSON을 처리하는 데에 아주 유용한 기능을 가지고 있기 때문에 Ruby 프로그래밍을 배우는 데에는 JSON 이해가 필수적입니다.

## 하는 방법

우선 JSON 파일을 로드하기 위해 Ruby에서 제공하는 `JSON` 라이브러리를 `require` 한 후, `File.open` 메소드를 사용하여 파일을 열어 봅시다. 그런 다음 `JSON.load` 메소드를 통해 데이터를 읽어올 수 있습니다. 예를 들어, 아래 코드를 실행하면 JSON 파일의 내용을 `data` 변수에 저장할 수 있습니다.

```Ruby
require 'json'
file = File.open("sample.json")
data = JSON.load(file)
```

`data` 변수는 Ruby 해시(hash) 형식으로 데이터를 저장하므로, 여러분은 각 항목에 대해 일반적인 해시 연산을 사용할 수 있습니다. 예를 들어, `data["name"]`을 호출하면 JSON 파일에서 `name`에 해당하는 값을 가져올 수 있습니다. 또한 `data["pets"][0]["name"]`과 같은 방식으로 중첩된 값을 가져올 수 있습니다.

```Ruby
puts data["name"]
# output: "John Doe"

puts data["pets"][0]["name"]
# output: "Buddy"
```

## 깊이 파고들기

Ruby에서 JSON을 처리하는 데에는 더 많은 기능이 있습니다. 위와 같은 방법 외에도 `JSON.parse` 메소드를 사용하여 문자열 형식의 JSON 데이터를 해시 형식으로 변환할 수 있습니다. 또한 `JSON.generate` 메소드를 사용하여 해시를 JSON 형식의 문자열로 변환할 수 있습니다. 또한 Ruby에서는 `to_json` 메소드를 제공하여 객체를 JSON 형식의 문자열로 변환할 수 있습니다.

이 외에도 Ruby에서는 JSON을 처리하는 데에 유용한 다양한 메소드를 제공하고 있습니다. 이러한 메소드들을 사용하여 더욱 유연하고 효율적으로 JSON 데이터를 다룰 수 있습니다. 

## 함께 보기

다음은 Ruby에서 JSON을 처리하는 데에 관련된 유용한 링크들입니다:

- [JSON 라이브러리 문서](https://ruby-doc.org/stdlib-2.7.2/libdoc/json/rdoc/JSON.html)
- [Ruby와 JSON 데이터 파싱하기](https://www.rubyguides.com/2018/10/ruby-json-tutorial/)
- [Ruby와 JSON 변환하기](http://zetcode.com/ruby/json/)