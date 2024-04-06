---
date: 2024-01-20 17:46:44.622784-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C:) \uCEF4\uD4E8\uD130 \uC5B8\uC5B4\uB294\
  \ \uB370\uC774\uD130 \uCC98\uB9AC\uC758 \uAE30\uBCF8\uC801\uC778 \uBD80\uBD84\uC73C\
  \uB85C \uBB38\uC790\uC5F4 \uC870\uC791\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4. Ruby\uC5D0\
  \uC11C \uC11C\uBE0C\uC2A4\uD2B8\uB9C1\uC744 \uCD94\uCD9C\uD558\uB294 \uBC29\uBC95\
  \uC740 \uC2DC\uAC04\uC774 \uC9C0\uB098\uBA74\uC11C \uAC1C\uC120\uB418\uC5C8\uC2B5\
  \uB2C8\uB2E4. \uC608\uC804 \uBC29\uC2DD\uC5D0\uB294 \uAE30\uBCF8 \uBB38\uC790\uC5F4\
  \ \uBA54\uC11C\uB4DC\uB9CC \uC788\uC5C8\uC9C0\uB9CC, \uC9C0\uAE08\uC740 \uC815\uADDC\
  \ \uD45C\uD604\uC2DD\uACFC \uAC19\uC740 \uAC15\uB825\uD55C \uB3C4\uAD6C\uB3C4 \uC0AC\
  \uC6A9\uD560 \uC218\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.542821-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C:) \uCEF4\uD4E8\uD130 \uC5B8\uC5B4\uB294 \uB370\uC774\
  \uD130 \uCC98\uB9AC\uC758 \uAE30\uBCF8\uC801\uC778 \uBD80\uBD84\uC73C\uB85C \uBB38\
  \uC790\uC5F4 \uC870\uC791\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4."
title: "\uBD80\uBD84 \uBB38\uC790\uC5F4 \uCD94\uCD9C"
weight: 6
---

## How to: (어떻게:)
```ruby
# 주어진 문자열에 대한 서브스트링 추출
string = "Hello, Ruby!"

# 1. []와 범위 사용하기
substring = string[7,4]   # "Ruby"
puts substring            # => Ruby

# 2. slice 메소드 사용하기
substring = string.slice(0..4) # "Hello"
puts substring                 # => Hello

# 3. 정규 표현식 사용하기
substring = string[/[R].+/]    # "Ruby!"
puts substring                  # => Ruby!
```

## Deep Dive (심층 분석)
컴퓨터 언어는 데이터 처리의 기본적인 부분으로 문자열 조작을 포함합니다. Ruby에서 서브스트링을 추출하는 방법은 시간이 지나면서 개선되었습니다. 예전 방식에는 기본 문자열 메서드만 있었지만, 지금은 정규 표현식과 같은 강력한 도구도 사용할 수 있습니다. 대안으로는 `slice!`, `partition`, `rpartition`, 그리고 `unpack` 메소드가 있어 상황에 맞게 선택할 수 있습니다. 구현 세부 사항으로는 문자열이 인코딩 되는 방식과 내부에서 어떻게 메모리에 저장되는지 등이 있습니다. 이러한 세부 사항은 성능에 영향을 줄 수 있습니다.

## See Also (참고 자료)
- [Ruby-Doc: String class](https://ruby-doc.org/core-3.1.0/String.html)
- [Regular-Expressions.info: Ruby](https://www.regular-expressions.info/ruby.html)
