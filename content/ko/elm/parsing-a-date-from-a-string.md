---
title:                "문자열에서 날짜 추출하기"
html_title:           "Elm: 문자열에서 날짜 추출하기"
simple_title:         "문자열에서 날짜 추출하기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 무엇이며 왜?
날짜를 문자열로부터 분석하는 것은 프로그래머들이 로컬 앱에서 사용자가 제공한 날짜를 처리하고 날짜 관련 작업을 수행하는 데 필요합니다.

## 하는 방법:
`Elm/time` 라이브러리에서 제공하는 `parse` 함수를 사용하여 문자열로부터 날짜를 분석할 수 있습니다. 아래의 예제 코드는 `MM/dd/yyyy` 형식의 날짜를 분석하는 방법을 보여줍니다.

```Elm
import Time
import Maybe exposing (withDefault)

stringDate = "05/15/2020"

parsedDate = Time.parse Time.locale "MM/dd/yyyy" stringDate
  |> withDefault Time.millisecond
```

위 예제 코드에서 `parsedDate`는 `17,379,600,000` 밀리초로 파싱된 값을 가지고 있습니다.

## 깊이 파고들기:
날짜를 파싱하는 것은 사용자가 선호하는 형식으로 날짜를 표현하는 방식이 각기 다른 경우에 매우 중요합니다. 이전에는 개발자가 직접 날짜를 분석해야 했습니다. Elm은 이 과정을 간소화하고 개발자들이 날짜를 쉽게 처리할 수 있도록 도와줍니다. 또한 `String` 모듈을 사용하여 날짜를 문자열로 변환할 수도 있습니다.

## 관련 정보 보기:
- [Elm 공식 사이트](https://elm-lang.org/)
- [Elm/time 라이브러리의 `parse` 함수 문서](https://package.elm-lang.org/packages/elm/time/latest/Time#parse)
- [Elm 문자열 처리를 위한 `String` 모듈 문서](https://package.elm-lang.org/packages/elm/core/latest/String)