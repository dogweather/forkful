---
title:                "문자열의 길이 찾기"
html_title:           "Lua: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가?
문자열의 길이를 찾는 것은 문자열에 포함된 문자 수를 알아내는 과정입니다. 프로그래머가 이를 사용하는 이유는 검증, 정렬, 다른 연산 등에 필요한 중요한 정보를 제공하기 때문입니다.

## 어떻게 사용하는가?
```Ruby
str = "Hello World"
puts str.length
```
위 코드의 실행 결과는 `11`입니다. 여기서 `length` 메소드는 문자열에 있는 문자 수를 반환합니다.

## 자세한 설명
문자열의 길이를 찾는 방법은 프로그래밍의 기본 개념입니다. 오래전부터 사용되어 왔으며, 대부분의 프로그래밍 언어에 내장된 기능입니다.

대안 방식으로는 `size` 메소드를 사용할 수 있습니다. `length`와 `size`는 동일한 기능을 수행하지만, `size`는 일부 컬렉션 객체에 대해 사용할 수 있습니다.

```Ruby
str = "Hello World"
puts str.size
```
위 코드의 실행 결과는 또한 `11`입니다.

이러한 메소드의 내부적인 작동 방식은 문자열에서 각 문자를 확인하며 카운트하는 형식입니다.

## 참고 자료
- Ruby 공식 문서: [String](https://ruby-doc.org/core-2.7.0/String.html) 
- [Rubyist's Guide to String Methods](https://www.rubyguides.com/2018/01/ruby-string-methods/)
- [Ruby in Twenty Minutes](https://www.ruby-lang.org/en/documentation/quickstart/)