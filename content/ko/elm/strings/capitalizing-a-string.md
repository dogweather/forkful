---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:02.477568-07:00
description: "\uC5B4\uB5BB\uAC8C: Elm\uC5D0\uC11C\uB294 \uBB38\uC790\uC5F4\uC744 \uB300\
  \uBB38\uC790\uB85C \uB9CC\uB4DC\uB294 \uD2B9\uBCC4\uD55C \uB0B4\uC7A5 \uD568\uC218\
  \uAC00 \uC5C6\uC2B5\uB2C8\uB2E4. \uADF8\uB7EC\uB098 `toUpper`, `toLower`, `left`,\
  \ `dropLeft`\uACFC \uAC19\uC740 \uB0B4\uC7A5 `String` \uBAA8\uB4C8 \uD568\uC218\uB97C\
  \ \uC0AC\uC6A9\uD558\uC5EC \uC27D\uAC8C \uC774\uB97C \uB2EC\uC131\uD560 \uC218 \uC788\
  \uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.086896-06:00'
model: gpt-4-0125-preview
summary: "Elm\uC5D0\uC11C\uB294 \uBB38\uC790\uC5F4\uC744 \uB300\uBB38\uC790\uB85C\
  \ \uB9CC\uB4DC\uB294 \uD2B9\uBCC4\uD55C \uB0B4\uC7A5 \uD568\uC218\uAC00 \uC5C6\uC2B5\
  \uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uB300\uBB38\uC790\uD654"
weight: 2
---

## 어떻게:
Elm에서는 문자열을 대문자로 만드는 특별한 내장 함수가 없습니다. 그러나 `toUpper`, `toLower`, `left`, `dropLeft`과 같은 내장 `String` 모듈 함수를 사용하여 쉽게 이를 달성할 수 있습니다.

```elm
capitalize : String -> String
capitalize str =
    if String.isEmpty str then
        ""
    else
        String.toUpper (String.left 1 str) ++ String.toLower (String.dropLeft 1 str)

-- 예제 사용
main =
    String.toList "hello world" |> List.map capitalize |> String.join " "
    -- 출력: "Hello World"
```

더 복잡한 시나리오에 직면했거나 문자열을 대문자로 만드는 직접적인 방법을 제공하는 라이브러리를 사용하는 것을 선호한다면, `elm-community/string-extra`와 같은 타사 패키지를 고려해볼 수 있습니다. 그러나 마지막 업데이트 시점까지 Elm의 생태계는 언어와 프로젝트를 간결하게 유지하기 위해 이러한 작업을 내장 함수를 사용하여 처리할 것을 권장합니다.

```elm
import String.Extra as StringExtra

-- 타사 라이브러리에 `capitalize` 함수가 있는 경우
capitalizeWithLibrary : String -> String
capitalizeWithLibrary str =
    StringExtra.capitalize str

-- 가상 라이브러리 함수와 함께 사용하는 예제
main =
    "this is elm" |> capitalizeWithLibrary
    -- 가상 출력: "This is elm"
```

표준 라이브러리를 넘어서 추가 기능을 찾고 있다면, 문자열 조작을 위한 최신이며 가장 선호되는 라이브러리를 확인하기 위해 Elm 패키지 저장소를 항상 확인하세요.
