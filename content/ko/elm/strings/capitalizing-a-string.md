---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:02.477568-07:00
description: "\uBB38\uC790\uC5F4\uC758 \uCCAB \uAE00\uC790\uB97C \uB300\uBB38\uC790\
  \uB85C \uBCC0\uD658\uD558\uACE0 \uB098\uBA38\uC9C0\uB294 \uC18C\uBB38\uC790\uB85C\
  \ \uC720\uC9C0\uD558\uB294 \uC791\uC5C5\uC744 \uB9D0\uD569\uB2C8\uB2E4. \uC774\uB294\
  \ \uD45C\uC900\uD654\uB41C \uD615\uC2DD\uC774\uB098 \uAC00\uB3C5\uC131 \uBAA9\uC801\
  \uC73C\uB85C \uC790\uC8FC \uC0AC\uC6A9\uB429\uB2C8\uB2E4. \uD2B9\uD788 \uC0AC\uC6A9\
  \uC790 \uC778\uD130\uD398\uC774\uC2A4\uB098 \uC0AC\uC6A9\uC790 \uC785\uB825\uC744\
  \ \uCC98\uB9AC\uD558\uACE0 \uD45C\uC2DC\uD560 \uB54C \uB370\uC774\uD130\uAC00 \uC77C\
  \uAD00\uB418\uAC8C \uD45C\uC2DC\uB418\uB3C4\uB85D \uBCF4\uC7A5\uD558\uAE30 \uC704\
  \uD574 \uD504\uB85C\uADF8\uB798\uBA38\uB294 \uC774 \uC791\uC5C5\uC744 \uC790\uC8FC\
  \u2026"
lastmod: '2024-03-13T22:44:55.086896-06:00'
model: gpt-4-0125-preview
summary: "\uBB38\uC790\uC5F4\uC758 \uCCAB \uAE00\uC790\uB97C \uB300\uBB38\uC790\uB85C\
  \ \uBCC0\uD658\uD558\uACE0 \uB098\uBA38\uC9C0\uB294 \uC18C\uBB38\uC790\uB85C \uC720\
  \uC9C0\uD558\uB294 \uC791\uC5C5\uC744 \uB9D0\uD569\uB2C8\uB2E4. \uC774\uB294 \uD45C\
  \uC900\uD654\uB41C \uD615\uC2DD\uC774\uB098 \uAC00\uB3C5\uC131 \uBAA9\uC801\uC73C\
  \uB85C \uC790\uC8FC \uC0AC\uC6A9\uB429\uB2C8\uB2E4. \uD2B9\uD788 \uC0AC\uC6A9\uC790\
  \ \uC778\uD130\uD398\uC774\uC2A4\uB098 \uC0AC\uC6A9\uC790 \uC785\uB825\uC744 \uCC98\
  \uB9AC\uD558\uACE0 \uD45C\uC2DC\uD560 \uB54C \uB370\uC774\uD130\uAC00 \uC77C\uAD00\
  \uB418\uAC8C \uD45C\uC2DC\uB418\uB3C4\uB85D \uBCF4\uC7A5\uD558\uAE30 \uC704\uD574\
  \ \uD504\uB85C\uADF8\uB798\uBA38\uB294 \uC774 \uC791\uC5C5\uC744 \uC790\uC8FC\u2026"
title: "\uBB38\uC790\uC5F4 \uB300\uBB38\uC790\uD654"
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
