---
title:                "패턴에 일치하는 문자 삭제"
aliases:
- ko/ruby/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:42:57.851509-07:00
model:                 gpt-4-1106-preview
simple_title:         "패턴에 일치하는 문자 삭제"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열에서 특정 패턴에 맞는 문자를 삭제하는 것은 데이터를 정제하거나 형식을 통일하는 데 유용합니다. 이는 가독성을 높이고, 필요하지 않은 정보를 제거하여 처리 속도를 개선하기 위해 프로그래머들이 자주 사용합니다.

## How to (어떻게 하나요?)
```Ruby
# 문자열에서 숫자를 삭제
str = "Ruby123 is fun456"
clean_str = str.gsub(/\d+/, '')
puts clean_str
# 출력: "Ruby is fun"

# 문자열에서 공백을 삭제
str = "   Ruby   programming!    "
trimmed_str = str.strip
puts trimmed_str
# 출력: "Ruby   programming!"

# 특정 문자만 삭제
str = "Hello, World!"
no_comma_str = str.delete(',')
puts no_comma_str
# 출력: "Hello World!"

# 여러 특정 문자 삭제
str = "Hello, World!?#"
no_punctuation_str = str.delete(',!?#')
puts no_punctuation_str
# 출력: "Hello World"
```

## Deep Dive (깊이 탐색)
문자열에서 특정 패턴을 삭제하는 기능은 Ruby의 초창기 버전부터 제공되었습니다. `gsub` 메소드와 `delete` 메소드는 각기 다른 환경에서 유용합니다. `gsub`는 정규 표현식을 사용하여 복잡한 패턴을 매칭하고 교체할 수 있지만, `delete`는 주어진 문자를 직접적으로 지울 수 있는 메소드입니다.

그 밖에도 정규 표현식과 문자열 메소드를 조합하여 사용할 수도 있습니다. 예를 들어, `Scan` 메소드로 특정 패턴에 해당하는 부분만 추출하고, 이를 배열로 변환하여 사용하는 것입니다.

Ruby 는 문자열 처리에 있어 매우 강력하며 `gsub`, `sub`, `delete`, `strip`, `split` 등 다양한 메소드를 제공합니다. 이러한 메소드들은 빠르고, 편리하게 문자열을 조작할 수 있도록 도와줍니다.

## See Also (더 보기)
- Ruby 문자열 작업에 대한 공식 문서: [Ruby Docs](https://ruby-doc.org/core-3.1.0/String.html)
- 정규 표현식 기초: [Ruby Regexp](https://ruby-doc.org/core-3.1.0/Regexp.html)
- Ruby Style Guide (코딩 컨벤션): [RuboCop](https://rubocop.org)
