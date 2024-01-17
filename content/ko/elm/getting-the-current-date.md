---
title:                "현재 날짜 가져오기"
html_title:           "Elm: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 무엇이며 왜?
현재 날짜를 얻는 것은, 컴퓨터 내부에서 현재 시간을 나타내는 것입니다. 프로그래머들은 이를 사용하여 프로그램 내에서 다양한 시간 기능을 구현하고, 사용자에게 실제 시간을 보여 줄 수 있습니다.

## 어떻게:
Elm에서 현재 날짜를 얻는 방법은 간단합니다. Date 모듈을 사용하면 됩니다.

```Elm
-- Date 모듈 임포트
import Date exposing (today)

-- 현재 날짜 가져오기
currentDate = today
```

이를 실행하면 다음과 같은 결과를 얻을 수 있습니다.

```Elm
{year = 2021, month = 3, day = 21}
```

## 심층 탐구:
현재 날짜를 얻는 것은 우리가 생각하는 것보다 더 복잡한 작업입니다. 예를 들어, 우리는 타임존 문제를 고려해야 하며, 다른 언어와 도시에 따라 다른 날짜 형식이 있을 수 있습니다. 또한 세계 표준시(UTC)를 기준으로 정확한 현재 날짜를 가져오는 것도 중요합니다.

현재 날짜를 얻는 다른 방법으로는 `Date.fromString` 함수를 사용하는 것이 있습니다. 이 함수는 지정된 포맷에 따라 문자열을 날짜로 변환해 줍니다. 또는 JavaScript에서 제공하는 `Date` 객체를 이용하여 현재 날짜를 가져올 수 있습니다.

## 참고 자료:
- [Elm 공식 문서 - Date 모듈](https://package.elm-lang.org/packages/elm/time/latest/Date)
- [JavaScript Date 객체](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)