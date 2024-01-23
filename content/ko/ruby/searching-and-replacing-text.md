---
title:                "텍스트 검색 및 교체"
date:                  2024-01-20T17:58:36.811411-07:00
model:                 gpt-4-1106-preview
simple_title:         "텍스트 검색 및 교체"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
텍스트 검색 및 바꾸기는 문자열에서 특정 단어나 패턴을 찾아 다른 것으로 대체하는 과정입니다. 프로그래머는 데이터 정제, 로그 업데이트, 코드 리팩토링 등을 위해 이 작업을 수행합니다.

## How to: (어떻게 할까요?)
Ruby에서 문자열 내에서 텍스트를 검색하고 바꾸는 기본적인 방법은 `sub`와 `gsub` 메소드를 사용하는 것입니다. `sub`는 첫 번째 발견된 것만 바꾼다면, `gsub`는 모든 발견된 것을 바꿉니다.

```Ruby
text = "I like Ruby. Ruby is fun."

# 첫 번째 'Ruby'를 'Python'으로 바꿉니다
replaced_once = text.sub('Ruby', 'Python')
puts replaced_once
# => I like Python. Ruby is fun.

# 모든 'Ruby'를 'Python'으로 바꿉니다
replaced_all = text.gsub('Ruby', 'Python')
puts replaced_all
# => I like Python. Python is fun.

# 정규 표현식을 사용하여 모든 소문자를 대문자로 바꿉니다
upcased = text.gsub(/[a-z]/, &:upcase)
puts upcased
# => I LIKE RUBY. RUBY IS FUN.
```

## Deep Dive (심층 분석)
텍스트를 검색하고 바꾸는 기능은 다양한 프로그래밍 언어에서 비슷하게 제공되고 있습니다. 이런 기능은 Unix에서 사용되던 `sed`라는 스트림 편집기에서 유래되었습니다. Ruby의 `sub`와 `gsub` 메소드는 String 클래스에 속해 있는데, 이들 메소드는 내부적으로 강력한 정규 표현식 엔진을 사용하여 패턴 매칭을 수행합니다. 정규 표현식은 텍스트를 처리할 때 굉장히 유용하며 복잡한 패턴을 정의할 수 있습니다.

## See Also (더 보기)
- Ruby 문서에서의 String#sub 및 String#gsub 메소드: [String#sub](https://ruby-doc.org/core-3.1.0/String.html#method-i-sub), [String#gsub](https://ruby-doc.org/core-3.1.0/String.html#method-i-gsub)
- 정규 표현식 관련 정보: [Regular-Expressions.info](https://www.regular-expressions.info/)
- Unix `sed` 스트림 편집기에 대한 배경: [GNU sed manual](https://www.gnu.org/software/sed/manual/sed.html)
