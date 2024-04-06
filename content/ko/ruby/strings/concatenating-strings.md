---
date: 2024-01-20 17:35:37.071841-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) \uBB38\uC790\uC5F4\
  \ \uC5F0\uACB0\uC740 \uB8E8\uBE44\uC5D0\uC11C \uC0C1\uB2F9\uD788 \uAE30\uBCF8\uC801\
  \uC778 \uAE30\uB2A5\uC785\uB2C8\uB2E4. \uC5ED\uC0AC\uC801\uC73C\uB85C, \uB8E8\uBE44\
  \uB294 \uB2E4\uB978 \uC5B8\uC5B4\uB4E4\uCC98\uB7FC '+', '<<', 'concat'\uC640 \uAC19\
  \uC740 \uBC29\uBC95\uB4E4\uC744 \uC81C\uACF5\uD574\uC654\uC2B5\uB2C8\uB2E4. '+'\
  \ \uC5F0\uC0B0\uC790\uB294 \uC0C8\uB85C\uC6B4 \uBB38\uC790\uC5F4 \uAC1D\uCCB4\uB97C\
  \ \uB9CC\uB4E4\uC9C0\uB9CC, '<<'\uB098 'concat'\uC740 \uC6D0\uBCF8\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:10.159554-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) \uBB38\uC790\uC5F4 \uC5F0\uACB0\
  \uC740 \uB8E8\uBE44\uC5D0\uC11C \uC0C1\uB2F9\uD788 \uAE30\uBCF8\uC801\uC778 \uAE30\
  \uB2A5\uC785\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uC5F0\uACB0\uD558\uAE30"
weight: 3
---

## How to: (어떻게 하나요?)
```Ruby
# 두 문자열을 '+'로 연결
greeting = "안녕" + "하세요"
puts greeting # => 안녕하세요

# '<<' 연산자를 사용하여 문자열에 추가
greeting << ", 루비!"
puts greeting # => 안녕하세요, 루비!

# 'concat' 메소드 사용
greeting.concat(" 잘 지내시나요?")
puts greeting # => 안녕하세요, 루비! 잘 지내시나요?
```

## Deep Dive (더 깊게 알아보기)
문자열 연결은 루비에서 상당히 기본적인 기능입니다. 역사적으로, 루비는 다른 언어들처럼 '+', '<<', 'concat'와 같은 방법들을 제공해왔습니다. '+' 연산자는 새로운 문자열 객체를 만들지만, '<<'나 'concat'은 원본 문자열을 변경합니다. 이는 메모리 사용에 영향을 미치기 때문에 특히 큰 데이터를 다룰 때 중요할 수 있죠. 대안으로, 'sprintf'나 'format' 메소드, 혹은 인터폴레이션(interpolation)("#{...}")을 사용할 수 있습니다. 이 방법들은 다양한 유형의 데이터를 효율적으로 문자열로 변환할 때 유용합니다.

## See Also (더 참고할 내용)
- Ruby Official Documentation for String Concatenation: [ruby-doc.org](https://ruby-doc.org/core-2.7.0/String.html#method-i-2B)
- An Interesting Read on String Efficiency in Ruby: [Practical Ruby Performance Optimization](http://shop.oreilly.com/product/0636920040877.do)
- Ruby Style Guide: [ruby-style-guide](https://rubystyle.guide/#string-interpolation)
