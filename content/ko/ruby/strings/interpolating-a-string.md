---
date: 2024-01-20 17:51:35.050185-07:00
description: "\uC2A4\uD2B8\uB9C1 \uC778\uD130\uD3F4\uB808\uC774\uC158\uC740 \uBB38\
  \uC790\uC5F4 \uC548\uC5D0 \uBCC0\uC218\uB098 \uD45C\uD604\uC2DD\uC758 \uACB0\uACFC\
  \uB97C \uC0BD\uC785\uD558\uB294 \uAC83\uC785\uB2C8\uB2E4. \uCF54\uB4DC\uB97C \uB354\
  \ \uC77D\uAE30 \uC27D\uACE0 \uC720\uC9C0\uBCF4\uC218\uD558\uAE30 \uC871\uD558\uAC8C\
  \ \uD558\uAE30 \uC704\uD574 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.971636-06:00'
model: gpt-4-1106-preview
summary: "\uC2A4\uD2B8\uB9C1 \uC778\uD130\uD3F4\uB808\uC774\uC158\uC740 \uBB38\uC790\
  \uC5F4 \uC548\uC5D0 \uBCC0\uC218\uB098 \uD45C\uD604\uC2DD\uC758 \uACB0\uACFC\uB97C\
  \ \uC0BD\uC785\uD558\uB294 \uAC83\uC785\uB2C8\uB2E4. \uCF54\uB4DC\uB97C \uB354 \uC77D\
  \uAE30 \uC27D\uACE0 \uC720\uC9C0\uBCF4\uC218\uD558\uAE30 \uC871\uD558\uAC8C \uD558\
  \uAE30 \uC704\uD574 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uBCF4\uAC04\uD558\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇인가요? 왜 사용하나요?)
스트링 인터폴레이션은 문자열 안에 변수나 표현식의 결과를 삽입하는 것입니다. 코드를 더 읽기 쉽고 유지보수하기 족하게 하기 위해 사용합니다.

## How to (어떻게 하나요?)
```Ruby
name = "세종대왕"
age = 615

# 문자열 인터폴레이션 사용 예제
greeting = "안녕하세요, #{name}님! 당신의 나이는 #{age}살이군요."
puts greeting
```
출력:
```
안녕하세요, 세종대왕님! 당신의 나이는 615살이군요.
```

## Deep Dive (심층 탐구)
스트링 인터폴레이션이라는 개념은 오래 전부터 프로그래밍에서 사용됐습니다. Ruby에서는 중괄호 내부에 `#{}` 구문을 사용해 구현됩니다. 이는 코드 내에서 문자열을 동적으로 조립할 때 강력한 도구입니다. `+`연산자나`concat` 메서드 등의 대안이 있긴 하지만, 가독성이나 퍼포먼스 면에서 인터폴레이션이 더 낫습니다. 내부적으로, Ruby는 이를 처리하기 위해 해당 변수나 표현식을 문자열로 변환하는 `to_s` 메서드를 자동으로 호출합니다.

## See Also (관련 자료)
- [The Pragmatic Programmers' Guide](http://ruby-doc.com/docs/ProgrammingRuby/)
