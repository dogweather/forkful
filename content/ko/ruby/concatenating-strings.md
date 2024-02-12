---
title:                "문자열 연결하기"
aliases:
- ko/ruby/concatenating-strings.md
date:                  2024-01-20T17:35:37.071841-07:00
model:                 gpt-4-1106-preview
simple_title:         "문자열 연결하기"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가요? 왜 필요한가요?)
문자열 연결은 개별 문자열들을 하나로 붙이는 것입니다. 프로그래머들은 정보를 조합하거나 출력 형태를 정리할 때 이 방법을 자주 사용합니다.

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
