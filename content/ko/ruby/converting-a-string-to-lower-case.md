---
title:                "문자열을 소문자로 변환하기"
html_title:           "Bash: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 필요한가?
문자열을 소문자로 변환한다는 것은 모든 대문자를 해당하는 소문자로 바꾸는 작업을 의미합니다. 이 작업은 간혹 대소문자를 구분하지 않는 검색이나 비교를 수행할 때 유용합니다.

## 코드 작성 방법 :
Ruby에서는 `downcase` 메소드를 이용하여 이 작업을 손쉽게 수행할 수 있습니다.

```Ruby
str = "Hello, World!"
lower_case_str = str.downcase
puts lower_case_str
```
출력 결과 :

```Ruby
"hello, world!"
```

## 깊이 있는 정보 :
소문자 변환은 문자열의 각 문자를 해당하는 ASCII 코드를 통해 변환하는 간단한 작업입니다. 이런 변환 원리를 이해하면, 대소문자 변환 이외의 다양한 문자열 변환이나 조작 등에 대한 이해가 높아집니다.

Ruby에서는 또한 `downcase!` 메소드를 제공하는데, 이 메소드는 원본 문자열 자체를 소문자로 변환하는 일종의 파괴적인 연산자입니다. 

```Ruby
str = "Hello, Ruby!"
str.downcase!
puts str
```

출력 결과 :

```Ruby
"hello, ruby!"
```

## 참고 자료 :
1. Ruby Doc - downcase: https://ruby-doc.org/core-2.7.1/String.html#method-i-downcase
2. Stackoverflow - How to make a string all lower or uppercase: https://stackoverflow.com/questions/5032356/in-ruby-how-do-i-convert-a-string-to-lowercase