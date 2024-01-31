---
title:                "문자열을 소문자로 변환하기"
date:                  2024-01-20T17:38:10.001335-07:00
model:                 gpt-4-1106-preview
simple_title:         "문자열을 소문자로 변환하기"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가? 그리고 왜?)
문자열을 소문자로 변환한다는 것은, 모든 대문자를 소문자로 바꾸는 과정입니다. 프로그래머들은 대소문자 구분 없이 데이터를 비교하거나 정렬할 필요가 있을 때 이 작업을 합니다.

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
