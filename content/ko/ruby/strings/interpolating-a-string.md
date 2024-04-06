---
date: 2024-01-20 17:51:35.050185-07:00
description: "How to (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) \uC2A4\uD2B8\uB9C1 \uC778\
  \uD130\uD3F4\uB808\uC774\uC158\uC774\uB77C\uB294 \uAC1C\uB150\uC740 \uC624\uB798\
  \ \uC804\uBD80\uD130 \uD504\uB85C\uADF8\uB798\uBC0D\uC5D0\uC11C \uC0AC\uC6A9\uB410\
  \uC2B5\uB2C8\uB2E4. Ruby\uC5D0\uC11C\uB294 \uC911\uAD04\uD638 \uB0B4\uBD80\uC5D0\
  \ `#{}` \uAD6C\uBB38\uC744 \uC0AC\uC6A9\uD574 \uAD6C\uD604\uB429\uB2C8\uB2E4. \uC774\
  \uB294 \uCF54\uB4DC \uB0B4\uC5D0\uC11C \uBB38\uC790\uC5F4\uC744 \uB3D9\uC801\uC73C\
  \uB85C \uC870\uB9BD\uD560 \uB54C \uAC15\uB825\uD55C \uB3C4\uAD6C\uC785\uB2C8\uB2E4\
  . `+`\uC5F0\uC0B0\uC790\uB098`concat` \uBA54\uC11C\uB4DC\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:10.152607-06:00'
model: gpt-4-1106-preview
summary: "Ruby\uC5D0\uC11C\uB294 \uC911\uAD04\uD638 \uB0B4\uBD80\uC5D0 `#{}` \uAD6C\
  \uBB38\uC744 \uC0AC\uC6A9\uD574 \uAD6C\uD604\uB429\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uBCF4\uAC04\uD558\uAE30"
weight: 8
---

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
