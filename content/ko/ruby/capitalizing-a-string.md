---
title:                "문자열 대문자화"
aliases:
- ko/ruby/capitalizing-a-string.md
date:                  2024-02-03T19:06:14.657483-07:00
model:                 gpt-4-0125-preview
simple_title:         "문자열 대문자화"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
