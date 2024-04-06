---
date: 2024-01-20 17:38:10.001335-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) \uBB38\uC790\uC5F4\uC744\
  \ \uC18C\uBB38\uC790\uB85C \uBC14\uAFB8\uB294 \uAE30\uB2A5\uC740 \uCEF4\uD4E8\uD305\
  \uC758 \uCD08\uAE30 \uB2E8\uACC4\uBD80\uD130 \uC788\uC5C8\uC2B5\uB2C8\uB2E4. \uC5ED\
  \uC0AC\uC801\uC73C\uB85C, \uB370\uC774\uD130 \uCC98\uB9AC\uAC00 \uC218\uB3D9\uC801\
  \uC77C \uB54C \uB300\uC18C\uBB38\uC790\uB97C \uAD6C\uBD84\uD558\uC9C0 \uC54A\uB294\
  \ \uAC80\uC0C9\uC774 \uD544\uC218\uC801\uC774\uC5C8\uC8E0. Elm\uC5D0\uC11C `String.toLower`\
  \ \uD568\uC218\uB294 \uB0B4\uBD80\uC801\uC73C\uB85C \uBAA8\uB4E0 \uB300\uBB38\uC790\
  \ \uC720\uB2C8\uCF54\uB4DC \uBB38\uC790\uB97C\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:09.461788-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) \uBB38\uC790\uC5F4\uC744 \uC18C\
  \uBB38\uC790\uB85C \uBC14\uAFB8\uB294 \uAE30\uB2A5\uC740 \uCEF4\uD4E8\uD305\uC758\
  \ \uCD08\uAE30 \uB2E8\uACC4\uBD80\uD130 \uC788\uC5C8\uC2B5\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\uD558\uAE30"
weight: 4
---

## How to: (어떻게 하나요?)
```Elm
import String

-- 문자열을 소문자로 변환하는 예제
lowerCaseString : String -> String
lowerCaseString str =
    String.toLower str

-- 변환 예제 사용
example : String
example =
    lowerCaseString "HELLO, WORLD!"

-- 출력 예제: "hello, world!"
```

## Deep Dive (심층 분석)
문자열을 소문자로 바꾸는 기능은 컴퓨팅의 초기 단계부터 있었습니다. 역사적으로, 데이터 처리가 수동적일 때 대소문자를 구분하지 않는 검색이 필수적이었죠. Elm에서 `String.toLower` 함수는 내부적으로 모든 대문자 유니코드 문자를 해당 소문자로 바꿔줍니다.

임베디드 시스템이나 리소스에 제약이 있는 환경에서는 자체 소문자 변환 함수를 구현해서 메모리 사용을 최적화 할 수도 있습니다. 그러나 대부분의 현대적인 어플리케이션에서는 Elm 표준 라이브러리의 `String.toLower`를 사용하는 것이 가장 쉬운 방법입니다.

## See Also (관련 자료)
- Elm `String` 모듈 문서: [String toLower](https://package.elm-lang.org/packages/elm/core/latest/String#toLower)
- 유니코드 문자 세트와 대소문자 매핑: [Unicode Case Folding](https://unicode.org/reports/tr21/tr21-5.html)
