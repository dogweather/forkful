---
title:                "정규 표현식 사용하기"
html_title:           "Ruby: 정규 표현식 사용하기"
simple_title:         "정규 표현식 사용하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜
정규 표현식을 사용하는 이유는 텍스트에서 특정한 규칙을 가진 패턴을 찾거나 대체하거나 추출하기 위해서입니다.

## 사용 방법
정규 표현식은 Ruby에서 \와 일부 문자를 사용하여 패턴을 지정하는 형식입니다. 예를 들어, 텍스트에서 특정한 문자열을 찾을 때는 `/\bapple\b/`과 같이 사용합니다.
```Ruby
# 예제 1: 텍스트에서 공백으로 나눠진 단어 찾기
text = "Hello, my name is Ruby!"
p text.scan(/\b\w+\b/)  # ["Hello", "my", "name", "is", "Ruby"]

# 예제 2: 특정 문자열 대체하기
text = "I love apples"
text.gsub!(/apples/, "oranges")
p text  # "I love oranges"
```

## 깊이 있는 설명
정규 표현식은 굉장히 강력하지만, 조금 복잡하고 어려울 수도 있습니다. 이를 효과적으로 사용하기 위해서는 패턴을 잘 이해하고 구문을 숙지하는 것이 중요합니다. 또한, Ruby에서 지원하는 여러 기능을 사용하여 더 유연한 정규 표현식을 만들 수도 있습니다.

## 참고 자료
- 정규 표현식 문법: https://docs.ruby-lang.org/en/master/doc/regexp_rdoc.html
- Rubular - 정규 표현식 연습 사이트: https://rubular.com/
- Regex Tutorial - 정규 표현식 튜토리얼: https://www.regular-expressions.info/tutorial.html