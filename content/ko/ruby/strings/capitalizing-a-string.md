---
changelog:
- 2024-03-25, dogweather, edited and tested
- 2024-03-25, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:02:29.358527-07:00
description: "\uBB38\uC790\uC5F4\uC758 \uCCAB \uAE00\uC790\uB97C \uB300\uBB38\uC790\
  \uB85C \uBCC0\uD658\uD558\uACE0 \uB098\uBA38\uC9C0\uB97C \uC18C\uBB38\uC790\uB85C\
  \ \uBCC0\uD658\uD558\uB294 \uAC83\uC774 \uBCF4\uD1B5 \uBB38\uC790\uC5F4\uC758 \uB300\
  \uBB38\uC790\uD654\uB97C \uB73B\uD569\uB2C8\uB2E4. \uD558\uC9C0\uB9CC \uB54C\uB54C\
  \uB85C, \uC774\uB294 \uADF8\uC800 \uBB38\uC790\uC5F4\uC758 \uCCAB \uAE00\uC790\uB97C\
  \ \uB300\uBB38\uC790\uB85C \uB9CC\uB4E4\uBA74\uC11C \uB098\uBA38\uC9C0 \uBB38\uC790\
  \uC5F4\uC740 \uBCC0\uACBD\uD558\uC9C0 \uC54A\uB294 \uAC83\uC744 \uC758\uBBF8\uD560\
  \ \uC218\uB3C4 \uC788\uC2B5\uB2C8\uB2E4. \uC194\uC9C1\uD788, \uC81C \uC0DD\uAC01\
  \uC5D0 \uC774\uB294 \uB2E4\uC18C \uBAA8\uD638\uD55C \uC6A9\uC5B4\uC785\uB2C8\uB2E4\
  ."
lastmod: '2024-03-25T19:22:12.711212-06:00'
model: gpt-4-0125-preview
summary: "\uBB38\uC790\uC5F4\uC758 \uCCAB \uAE00\uC790\uB97C \uB300\uBB38\uC790\uB85C\
  \ \uBCC0\uD658\uD558\uACE0 \uB098\uBA38\uC9C0\uB97C \uC18C\uBB38\uC790\uB85C \uBCC0\
  \uD658\uD558\uB294 \uAC83\uC774 \uBCF4\uD1B5 \uBB38\uC790\uC5F4\uC758 \uB300\uBB38\
  \uC790\uD654\uB97C \uB73B\uD569\uB2C8\uB2E4. \uD558\uC9C0\uB9CC \uB54C\uB54C\uB85C\
  , \uC774\uB294 \uADF8\uC800 \uBB38\uC790\uC5F4\uC758 \uCCAB \uAE00\uC790\uB97C \uB300\
  \uBB38\uC790\uB85C \uB9CC\uB4E4\uBA74\uC11C \uB098\uBA38\uC9C0 \uBB38\uC790\uC5F4\
  \uC740 \uBCC0\uACBD\uD558\uC9C0 \uC54A\uB294 \uAC83\uC744 \uC758\uBBF8\uD560 \uC218\
  \uB3C4 \uC788\uC2B5\uB2C8\uB2E4. \uC194\uC9C1\uD788, \uC81C \uC0DD\uAC01\uC5D0 \uC774\
  \uB294 \uB2E4\uC18C \uBAA8\uD638\uD55C \uC6A9\uC5B4\uC785\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uB300\uBB38\uC790\uD654"
---

## 무엇 & 왜?
문자열의 첫 글자를 대문자로 변환하고 나머지를 소문자로 변환하는 것이 보통 문자열의 대문자화를 뜻합니다. 하지만 때때로, 이는 그저 문자열의 첫 글자를 대문자로 만들면서 나머지 문자열은 변경하지 않는 것을 의미할 수도 있습니다. 솔직히, 제 생각에 이는 다소 모호한 용어입니다.

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
