---
title:                "날짜를 문자열로 변환하기"
date:                  2024-01-20T17:36:20.245090-07:00
model:                 gpt-4-1106-preview
simple_title:         "날짜를 문자열로 변환하기"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
날짜를 문자열로 변환하는 것은 일정이나 기록을 읽기 쉬운 형태로 표현하기 위해 사용합니다. 프로그래머들은 사용자 인터페이스나 데이터 저장을 위해 이런 변환을 자주 사용합니다.

## 실습 방법:
Elm에서 날짜를 문자열로 변환하기 위해 `elm/time` 모듈과 `Time` 패키지를 활용합니다.

```Elm
import Time exposing (Posix)
import Time.Zone exposing (Zone)

-- 날짜를 문자열로 변환하는 함수를 정의합시다.
formatDate : Zone -> Posix -> String
formatDate zone posix =
    Time.toUtc zone posix
        |> Time.posixToMillis
        |> Time.millisToPosix
        |> Time.toIsoString
        |> String.left 10

-- 예제 사용
example : Zone -> String
example zone =
    formatDate zone (Time.millisToPosix 1582531223673)

-- sample output: "2020-02-24"
```

위 코드는 UTC로 변환 후 ISO 형식 날짜 문자열로 만들고, 원하는 형식에 맞추어 자릅니다.

## 깊이 있게 파기:
날짜를 문자열로 변환하는 기능은 Elm이 나오기 이전부터 필수적이었습니다. JavaScript에서는 `Date` 객체와 `toLocaleDateString` 같은 메소드를 이용했는데, Elm에서는 `Time` 모듈을 통해 보다 명시적인 처리를 할 수 있습니다.

Elm에서는 시간대(`Zone`)를 명확히 다루면서 변환 과정을 더욱 투명하게 만듭니다. 다양한 시간대를 지원하여 국제적인 사용자들에게 맞춘 서비스 제공이 가능합니다.

ISO 문자열 변환 외에도, 사용자 정의 형식을 사용하여 다양한 날짜 형식을 지원할 수 있습니다. 하지만 Elm은 기본적으로 ISO 형식을 지향하며, 이는 데이터 교환의 표준 형식입니다.

## 참고 자료:
- Elm 시간(Time) 패키지 문서: [https://package.elm-lang.org/packages/elm/time/latest/](https://package.elm-lang.org/packages/elm/time/latest/)
- Elm 포럼 및 커뮤니티에서의 시간 관련 토론 [https://discourse.elm-lang.org/](https://discourse.elm-lang.org/)
- ISO 8601 날짜 및 시간 표준에 대한 참조: [https://en.wikipedia.org/wiki/ISO_8601](https://en.wikipedia.org/wiki/ISO_8601)
