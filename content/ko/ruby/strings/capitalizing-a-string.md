---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:14.657483-07:00
description: "\uD504\uB85C\uADF8\uB798\uBC0D\uC5D0\uC11C \uBB38\uC790\uC5F4\uC758\
  \ \uCCAB \uAE00\uC790\uB97C \uB300\uBB38\uC790\uB85C \uBCC0\uD658\uD558\uACE0 \uB098\
  \uBA38\uC9C0\uB97C \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\uD558\uB294 \uAC83\uC744\
  \ \uBCF4\uD1B5 \uBB38\uC790\uC5F4 \uB300\uBB38\uC790\uD654\uB77C\uACE0 \uD569\uB2C8\
  \uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uBA85\uBA85 \uADDC\uCE59\uC744\
  \ \uC900\uC218\uD558\uAC70\uB098 \uCD9C\uB825\uC744 \uB354 \uC77D\uAE30 \uC27D\uAC8C\
  \ \uB9CC\uB4E4\uAC70\uB098 \uBE44\uAD50 \uBC0F \uC800\uC7A5\uC744 \uC704\uD55C \uB370\
  \uC774\uD130 \uC77C\uAD00\uC131\uC744 \uBCF4\uC7A5\uD558\uAE30 \uC704\uD574 \uC774\
  \uB7EC\uD55C \uC791\uC5C5\uC744 \uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.967374-06:00'
model: gpt-4-0125-preview
summary: "\uD504\uB85C\uADF8\uB798\uBC0D\uC5D0\uC11C \uBB38\uC790\uC5F4\uC758 \uCCAB\
  \ \uAE00\uC790\uB97C \uB300\uBB38\uC790\uB85C \uBCC0\uD658\uD558\uACE0 \uB098\uBA38\
  \uC9C0\uB97C \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\uD558\uB294 \uAC83\uC744 \uBCF4\
  \uD1B5 \uBB38\uC790\uC5F4 \uB300\uBB38\uC790\uD654\uB77C\uACE0 \uD569\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uBA85\uBA85 \uADDC\uCE59\uC744 \uC900\
  \uC218\uD558\uAC70\uB098 \uCD9C\uB825\uC744 \uB354 \uC77D\uAE30 \uC27D\uAC8C \uB9CC\
  \uB4E4\uAC70\uB098 \uBE44\uAD50 \uBC0F \uC800\uC7A5\uC744 \uC704\uD55C \uB370\uC774\
  \uD130 \uC77C\uAD00\uC131\uC744 \uBCF4\uC7A5\uD558\uAE30 \uC704\uD574 \uC774\uB7EC\
  \uD55C \uC791\uC5C5\uC744 \uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uB300\uBB38\uC790\uD654"
---

{{< edit_this_page >}}

## 무엇과 왜?
프로그래밍에서 문자열의 첫 글자를 대문자로 변환하고 나머지를 소문자로 변환하는 것을 보통 문자열 대문자화라고 합니다. 프로그래머들은 명명 규칙을 준수하거나 출력을 더 읽기 쉽게 만들거나 비교 및 저장을 위한 데이터 일관성을 보장하기 위해 이러한 작업을 합니다.

## 어떻게 하나요?
루비는 문자열 조작을 위한 직관적인 메서드를 제공하며, 그 중에는 대문자화도 포함됩니다. 다음은 루비에서 문자열을 대문자화하는 방법입니다:

```ruby
# 루비의 내장 메서드
string = "hello world"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

루비의 `.capitalize` 메소드는 편리하지만 첫 글자에만 영향을 미칩니다. 문자열의 각 단어를 대문자화하려면(즉, 타이틀 케이스로 만들려면) 레일즈의 ActiveSupport 확장에서 `titleize` 메소드를 사용하거나 직접 구현할 수도 있습니다:

```ruby
# 레일즈에서 ActiveSupport의 'titleize' 사용
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

레일즈를 사용하지 않거나 순수 루비 솔루션을 선호한다면, 문자열의 각 단어를 대문자화하는 방법은 다음과 같습니다:

```ruby
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

이 방법은 문자열을 단어의 배열로 분할한 다음 각각을 대문자화하여 공백으로 다시 연결합니다.
