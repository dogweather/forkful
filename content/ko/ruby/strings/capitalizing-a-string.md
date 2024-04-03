---
changelog:
- 2024-03-25, dogweather, edited and tested
- 2024-03-25, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:02:29.358527-07:00
description: "\uC5B4\uB5BB\uAC8C: \uB8E8\uBE44\uB294 \uD3EC\uD568\uD55C [\uBB38\uC790\
  \uC5F4 \uC870\uC791\uC744 \uC704\uD55C \uC9C1\uAD00\uC801\uC778 \uBA54\uC18C\uB4DC\
  ](https://docs.ruby-lang.org/en/3.3/String.html)\uB97C \uC81C\uACF5\uD569\uB2C8\uB2E4\
  ."
lastmod: '2024-03-25T19:22:12.711212-06:00'
model: gpt-4-0125-preview
summary: "\uB8E8\uBE44\uB294 \uD3EC\uD568\uD55C [\uBB38\uC790\uC5F4 \uC870\uC791\uC744\
  \ \uC704\uD55C \uC9C1\uAD00\uC801\uC778 \uBA54\uC18C\uB4DC](https://docs.ruby-lang.org/en/3.3/String.html)\uB97C\
  \ \uC81C\uACF5\uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uB300\uBB38\uC790\uD654"
weight: 2
---

## 어떻게:
루비는 포함한 [문자열 조작을 위한 직관적인 메소드](https://docs.ruby-lang.org/en/3.3/String.html)를 제공합니다:

```ruby
# 루비의 내장 메소드
string = "hello WORLD"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

매우 편리합니다.

루비의 `.capitalize` 메소드는 편리하지만 첫 글자만 대문자로 만듭니다. 더 많은 제어를 원하거나 문자열의 각 단어를 대문자화하려면(타이틀 케이스라고 알려져 있음), Rails의 ActiveSupport 확장에 있는 `titleize` 메소드를 사용하거나 직접 구현할 수 있습니다:

```ruby
# Rails에서 ActiveSupport의 'titleize' 사용
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

```ruby
# 직접 만든 솔루션
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

이 메소드는 문자열을 단어 배열로 분할한 다음, 각 단어를 대문자화하고 다시 공백으로 조인합니다.

개인적으로, 이 아이디어를 제 코드에서 훨씬 더 발전시켰습니다. ["a"와 "the" 같은 작은 단어들을 고려한 나만의 `titleize` 메소드를 작성했습니다](https://github.com/public-law/law_string/blob/master/lib/law_string.rb).
