---
title:                "문자열의 길이 찾기"
html_title:           "Ruby: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 무엇이고 왜?

문자열의 길이를 찾는 것은 문자열에 있는 문자의 개수를 결정하는 것입니다. 이것은 문자열의 크기 또는 길이를 알고 싶을 때 유용합니다. 프로그래머들은 이것을 자주 사용하는데, 예를 들어서 사용자의 입력값이 특정 길이와 일치하는지 확인할 때 사용할 수 있습니다.

## 어떻게?

```Ruby
puts "Hello, world!".length # 13
```
위의 코드에서는 "Hello, world!" 문자열의 길이를 찾아서 출력하는 예제를 볼 수 있습니다.

```Ruby
my_string = "This is a sample string"
puts my_string.length # 23
```
또한 위의 예제와 같이 변수에 문자열을 할당하고 그 변수의 길이를 찾아서 출력할 수도 있습니다.

## 깊이 파고들기

**역사적 배경**: 문자열의 길이를 찾는 방법은 프로그래밍 언어에 따라 다를 수 있습니다. 오래된 프로그래밍 언어들은 문자열 길이를 찾기 위해서 여러가지 방법을 사용했습니다. 하지만 현재의 Ruby 언어에서는 쉽고 간단한 `.length` 메소드를 이용하여 문자열의 길이를 찾을 수 있습니다.

**대안들**: Ruby 뿐만 아니라 다른 언어들에서도 문자열의 길이를 찾는 메소드 혹은 함수가 존재합니다. 예를 들어서 Python에서는 `len()` 함수를 이용하여 문자열의 길이를 찾을 수 있습니다.

**구현 세부사항**: Ruby의 `String` 클래스 내부에서 `.length` 메소드가 어떻게 구현되었는지를 알고 싶다면, Ruby 소스 코드를 참고해보시길 바랍니다.

## 관련 자료

- [Ruby 문서 - String 클래스](https://ruby-doc.org/core-2.7.0/String.html)
- [Ruby 소스 코드](https://github.com/ruby/ruby)