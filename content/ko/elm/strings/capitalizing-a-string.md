---
title:                "문자열 대문자화"
aliases:
- /ko/elm/capitalizing-a-string/
date:                  2024-02-03T19:05:02.477568-07:00
model:                 gpt-4-0125-preview
simple_title:         "문자열 대문자화"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇인가 & 왜인가?

문자열의 첫 글자를 대문자로 변환하고 나머지는 소문자로 유지하는 작업을 말합니다. 이는 표준화된 형식이나 가독성 목적으로 자주 사용됩니다. 특히 사용자 인터페이스나 사용자 입력을 처리하고 표시할 때 데이터가 일관되게 표시되도록 보장하기 위해 프로그래머는 이 작업을 자주 수행합니다.

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
