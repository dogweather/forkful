---
title:                "정규 표현식 사용"
html_title:           "Ruby: 정규 표현식 사용"
simple_title:         "정규 표현식 사용"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
정규 표현식 사용이 무엇인지와 프로그래머들이 왜 이를 사용하는지에 대해서 설명해 드리겠습니다. 정규 표현식은 문자열에서 일치하는 패턴을 찾거나 조작하는 데 사용되는 패턴 매칭 도구입니다. 프로그래머들은 정규 표현식을 사용하여 특정한 형식의 문자열을 검색하거나 필요에 맞게 수정할 수 있습니다.

## 어떻게:
아래는 Ruby에서 정규 표현식을 사용하는 간단한 예시와 결과입니다.

```Ruby
str = "Hello, Ruby!"

# "Hello"라는 단어가 있는지 확인
puts str =~ /Hello/ # 0

# "Bye"라는 단어가 있는지 확인
puts str =~ /Bye/ # nil

# 문자열에서 대문자만 추출
puts str.scan(/[A-Z]/) # ["H", "R"]
```

## 심층 분석:
정규 표현식은 1950년대에 수학자 Stephen Kleene에 의해 개발되었습니다. 당시에는 문자열 처리를 위한 강력한 도구로 소개되었고, 현재에도 프로그래밍에서 널리 사용되고 있습니다. Ruby에서도 문자열 처리를 위한 내장된 정규 표현식 메소드가 있지만 다른 대안으로 라이브러리를 사용할 수도 있습니다. 정규 표현식은 강력한 것이지만, 복잡한 패턴을 다룰 때는 이해하기 어려울 수 있으므로 항상 코드를 쉽게 읽고 이해할 수 있도록 주석을 추가하는 것이 좋습니다.

## 참고자료:
- [Ruby의 정규 표현식 도큐먼트](https://ruby-doc.org/core-2.7.2/Regexp.html)
- [정규 표현식 연습 사이트](https://rubular.com/)
- [정규 표현식 튜토리얼](https://www.regular-expressions.info/tutorial.html)