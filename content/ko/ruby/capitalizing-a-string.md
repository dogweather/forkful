---
title:                "문자열 대문자로 변환하기"
date:                  2024-01-19
html_title:           "Arduino: 문자열 대문자로 변환하기"
simple_title:         "문자열 대문자로 변환하기"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가요? & 왜 사용하나요?)
문자열의 첫 글자를 대문자로 만드는 걸 'capitalizing'이라고 합니다. 이건 문장을 시작하거나 중요한 단어를 강조할 때 주로 사용됩니다.

## How to (어떻게 하나요?)
Ruby에서 문자열의 첫 글자를 대문자로 바꾸는 방법은 여러 가지가 있습니다. 간단히 `capitalize` 메소드를 사용할 수 있죠. 보시죠:

```ruby
puts "hello world".capitalize
# 출력: Hello world

puts "ruby programming".split.map(&:capitalize).join(' ')
# 출력: Ruby Programming
```

## Deep Dive (심층 분석)
문자열을 대문자로 만드는 건 프로그래밍에서 오래된 전통 중 하나입니다. 이전에는 C 같은 낮은 레벨의 언어에서 직접 ASCII 코드를 조작하여 대문자로 변환했죠. Ruby에서는 `capitalize` 메소드가 하나의 문자열에 대해서만 작동합니다. 첫 단어만 대문자로 만들죠. 모든 단어를 대문자로 시작하려면 `split`, `map`, 그리고 `join` 메소드를 체인으로 연결해야 합니다.

보통 `capitalize`는 문자열의 첫글자를 대문자로, 나머지 글자는 소문자로 변환합니다. 다른 언어나 프레임워크에서는 이 과정이 다를 수 있으니 주의하세요.

대안으로 `titleize`, `upcase`, `downcase` 등의 메소드도 있습니다. `titleize`는 활용은 적지만, 모든 단어를 대문자로 시작하게 바꿉니다 (액티브서포트 라이브러리 필요). `upcase`는 모든 문자를 대문자로, `downcase`는 모두 소문자로 만듭니다.

구현 세부 사항을 조금 더 살펴보면, Ruby에서 문자열은 `String` 객체이고, 이 객체 안에는 문자열과 관련된 다양한 메소드가 있습니다. 이들 메소드는 객체를 직접 변형하거나 (mutate) 새로운 문자열을 반환하죠 (`capitalize!` 같은 뱅 메소드는 객체를 직접 변경합니다).

## See Also (관련 링크)
- Ruby의 공식 문서에서 [`String#capitalize`](https://ruby-doc.org/core-3.1.0/String.html#method-i-capitalize)에 대해 자세히 알아보세요.
- [`String`](https://ruby-doc.org/core-3.1.0/String.html) 클래스의 다양한 메소드를 확인할 수 있습니다.
- Ruby on Rails에서 사용되는 [`String#titleize`](https://api.rubyonrails.org/classes/String.html#method-i-titleize) 메소드.
- 문자열 처리를 위한 더 깊은 이해를 위해 [Ruby's RegExp](https://ruby-doc.org/core-3.1.0/Regexp.html) 를 탐구하는 것도 훌륭한 자료가 될 수 있습니다.
