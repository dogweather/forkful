---
title:                "문자열 대문자로 변환하기"
html_title:           "Ruby: 문자열 대문자로 변환하기"
simple_title:         "문자열 대문자로 변환하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 무엇이고 왜: 
문자열을 대문자로 변환하는 것은 문자열의 모든 문자를 대문자로 변경하는 것을 말합니다. 프로그래머들은 일반적으로 문자열을 대문자로 변환하여 정렬된 데이터를 비교하거나 출력하는데 사용합니다. 또는 사용자 입력을 제한하는 데에도 사용할 수 있습니다.

## 방법:
```Ruby
string = "hello world"
puts string.capitalize
```
출력: "Hello world"

```Ruby
string = "ruby is awesome"
puts string.upcase
```
출력: "RUBY IS AWESOME"

## 깊이 들어가기:
대문자로 변환하는 방법은 정규식을 사용하여 문자열의 문자 하나하나를 검사하고 대문자로 변경하는 것입니다. 다른 대문자 변환 방법으로는 String#capitalize, String#upcase 등이 있습니다. 이들은 각각 첫 번째 문자만 대문자로 변환하거나 문자열의 모든 문자를 대문자로 변환합니다.

## 관련 내용:
- [String#capitalize 메소드 문서 (공식 Ruby 문서)](https://ruby-doc.org/core-2.7.0/String.html#method-i-capitalize)
- [String#upcase 메소드 문서 (공식 Ruby 문서)](https://ruby-doc.org/core-2.7.0/String.html#method-i-upcase)
- [정규식에 대한 자세한 내용 (정규식을 사용하는 대부분의 프로그래밍 언어에서 유사하게 적용됩니다.)](https://ko.wikipedia.org/wiki/%EC%A0%95%EA%B7%9C%EC%8B%9D)