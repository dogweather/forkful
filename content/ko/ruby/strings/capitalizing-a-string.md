---
title:                "문자열 대문자화"
date:                  2024-03-25T17:32:00.769071-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-25, dogweather, edited and tested
  - 2024-03-25, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?
문자열의 첫 글자를 대문자로 변환하고 나머지를 소문자로 바꾸는 것이 보통 문자열의 첫 글자를 대문자화하는 것을 의미합니다. 그러나 때로는 첫 문자만 대문자로 만들고 나머지 문자열은 변경하지 않는 것을 의미할 수도 있습니다. 솔직히 제 의견으로는 다소 모호한 용어입니다.

## 어떻게:
루비는 문자열 대문자화를 포함하여 [문자열 조작을 위한 간단한 메서드들을 제공합니다](https://docs.ruby-lang.org/en/3.3/String.html):

```ruby
# 루비의 내장 메서드
string = "hello WORLD"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

매우 편리합니다.

루비의 `.capitalize` 메서드는 편리하지만 첫 글자만 대문자로 만듭니다. 문자열의 각 단어를 대문자화하는 것(타이틀 케이스라고 함)을 포함하여 더 많은 제어가 필요하다면, 레일즈의 ActiveSupport 확장에서 제공하는 `titleize` 메서드를 사용하거나 직접 구현할 수 있습니다:

```ruby
# 레일즈의 'titleize' 사용
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

```ruby
# 직접 만든 해결책
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

이 방법은 문자열을 단어의 배열로 분할한 다음 각각을 대문자화하고 그것들을 공백으로 다시 연결합니다.

개인적으로, 저는 이 아이디어를 제 코드에서 훨씬 더 발전시켰습니다. 저는 "a"와 "the" 같은 작은 단어들을 고려한 나만의 [`titleize` 메서드를 작성했습니다](https://github.com/public-law/law_string/blob/master/lib/law_string.rb).
