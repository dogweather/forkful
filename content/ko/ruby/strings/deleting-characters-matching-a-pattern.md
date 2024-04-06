---
date: 2024-01-20 17:42:57.851509-07:00
description: "How to (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) \uBB38\uC790\uC5F4\uC5D0\
  \uC11C \uD2B9\uC815 \uD328\uD134\uC744 \uC0AD\uC81C\uD558\uB294 \uAE30\uB2A5\uC740\
  \ Ruby\uC758 \uCD08\uCC3D\uAE30 \uBC84\uC804\uBD80\uD130 \uC81C\uACF5\uB418\uC5C8\
  \uC2B5\uB2C8\uB2E4. `gsub` \uBA54\uC18C\uB4DC\uC640 `delete` \uBA54\uC18C\uB4DC\uB294\
  \ \uAC01\uAE30 \uB2E4\uB978 \uD658\uACBD\uC5D0\uC11C \uC720\uC6A9\uD569\uB2C8\uB2E4\
  . `gsub`\uB294 \uC815\uADDC \uD45C\uD604\uC2DD\uC744 \uC0AC\uC6A9\uD558\uC5EC \uBCF5\
  \uC7A1\uD55C \uD328\uD134\uC744 \uB9E4\uCE6D\uD558\uACE0 \uAD50\uCCB4\uD560 \uC218\
  \ \uC788\uC9C0\uB9CC,\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.537186-06:00'
model: gpt-4-1106-preview
summary: "`gsub` \uBA54\uC18C\uB4DC\uC640 `delete` \uBA54\uC18C\uB4DC\uB294 \uAC01\
  \uAE30 \uB2E4\uB978 \uD658\uACBD\uC5D0\uC11C \uC720\uC6A9\uD569\uB2C8\uB2E4."
title: "\uD328\uD134\uC5D0 \uC77C\uCE58\uD558\uB294 \uBB38\uC790 \uC0AD\uC81C"
weight: 5
---

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
