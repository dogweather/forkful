---
date: 2024-01-20 17:58:36.811411-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD560\uAE4C\uC694?) Ruby\uC5D0\uC11C \uBB38\
  \uC790\uC5F4 \uB0B4\uC5D0\uC11C \uD14D\uC2A4\uD2B8\uB97C \uAC80\uC0C9\uD558\uACE0\
  \ \uBC14\uAFB8\uB294 \uAE30\uBCF8\uC801\uC778 \uBC29\uBC95\uC740 `sub`\uC640 `gsub`\
  \ \uBA54\uC18C\uB4DC\uB97C \uC0AC\uC6A9\uD558\uB294 \uAC83\uC785\uB2C8\uB2E4. `sub`\uB294\
  \ \uCCAB \uBC88\uC9F8 \uBC1C\uACAC\uB41C \uAC83\uB9CC \uBC14\uAFBC\uB2E4\uBA74,\
  \ `gsub`\uB294 \uBAA8\uB4E0 \uBC1C\uACAC\uB41C \uAC83\uC744 \uBC14\uAFC9\uB2C8\uB2E4\
  ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.970423-06:00'
model: gpt-4-1106-preview
summary: "Ruby\uC5D0\uC11C \uBB38\uC790\uC5F4 \uB0B4\uC5D0\uC11C \uD14D\uC2A4\uD2B8\
  \uB97C \uAC80\uC0C9\uD558\uACE0 \uBC14\uAFB8\uB294 \uAE30\uBCF8\uC801\uC778 \uBC29\
  \uBC95\uC740 `sub`\uC640 `gsub` \uBA54\uC18C\uB4DC\uB97C \uC0AC\uC6A9\uD558\uB294\
  \ \uAC83\uC785\uB2C8\uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uAC80\uC0C9 \uBC0F \uAD50\uCCB4"
weight: 10
---

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
