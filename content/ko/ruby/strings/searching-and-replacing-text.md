---
date: 2024-01-20 17:58:36.811411-07:00
description: "\uD14D\uC2A4\uD2B8 \uAC80\uC0C9 \uBC0F \uBC14\uAFB8\uAE30\uB294 \uBB38\
  \uC790\uC5F4\uC5D0\uC11C \uD2B9\uC815 \uB2E8\uC5B4\uB098 \uD328\uD134\uC744 \uCC3E\
  \uC544 \uB2E4\uB978 \uAC83\uC73C\uB85C \uB300\uCCB4\uD558\uB294 \uACFC\uC815\uC785\
  \uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB294 \uB370\uC774\uD130 \uC815\uC81C\
  , \uB85C\uADF8 \uC5C5\uB370\uC774\uD2B8, \uCF54\uB4DC \uB9AC\uD329\uD1A0\uB9C1 \uB4F1\
  \uC744 \uC704\uD574 \uC774 \uC791\uC5C5\uC744 \uC218\uD589\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.970423-06:00'
model: gpt-4-1106-preview
summary: "\uD14D\uC2A4\uD2B8 \uAC80\uC0C9 \uBC0F \uBC14\uAFB8\uAE30\uB294 \uBB38\uC790\
  \uC5F4\uC5D0\uC11C \uD2B9\uC815 \uB2E8\uC5B4\uB098 \uD328\uD134\uC744 \uCC3E\uC544\
  \ \uB2E4\uB978 \uAC83\uC73C\uB85C \uB300\uCCB4\uD558\uB294 \uACFC\uC815\uC785\uB2C8\
  \uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB294 \uB370\uC774\uD130 \uC815\uC81C, \uB85C\
  \uADF8 \uC5C5\uB370\uC774\uD2B8, \uCF54\uB4DC \uB9AC\uD329\uD1A0\uB9C1 \uB4F1\uC744\
  \ \uC704\uD574 \uC774 \uC791\uC5C5\uC744 \uC218\uD589\uD569\uB2C8\uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uAC80\uC0C9 \uBC0F \uAD50\uCCB4"
weight: 10
---

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
