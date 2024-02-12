---
title:                "부분 문자열 추출"
date:                  2024-01-20T17:46:44.622784-07:00
model:                 gpt-4-1106-preview
simple_title:         "부분 문자열 추출"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜?)
문자열에서 특정 부분을 뽑아내는 것을 "서브스트링 추출"이라 합니다. 데이터를 다룰 때 우리가 필요한 정보만 빠르게 찾기 위해 사용합니다.

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
