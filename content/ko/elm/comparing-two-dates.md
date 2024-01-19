---
title:                "두 날짜 비교하기"
html_title:           "C#: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하나?
날짜 비교는 두 날짜 값의 차이를 결정하는 프로세스입니다. 프로그래머들은 주로 어떤 이벤트가 먼저 일어났는지를 파악하거나, 시간 간격을 계산하는 데 이를 사용합니다.

## 어떻게 사용하나?
여기 Elm 코드는 두 날짜를 비교하는 방법을 보여줍니다.

```Elm
import Time exposing (Posix, diff, second, toTime)
import Time.Extra exposing (fromCalendarDate)


date1 : Posix
date1 =
    fromCalendarDate 2020 12 25


date2 : Posix
date2 =
    fromCalendarDate 2021 1 1


main =
    diff second date1 date2
        |> Debug.toString
        |> text
```

위 코드의 출력 값은 편차를 초로 나타낸 값입니다, `604800`.

## 깊이 파보기
Elm은 순수 함수적 언어이며, 시간과 같은 명령형 개념을 처리하는 독특한 방법이 있습니다. `Time.Posix`를 사용하거나 `Time.getTime` 함수를 통해 현재 시간을 얻을 수 있습니다.

`second`, `minute`, `hour` 등의 단위와 `diff` 함수를 사용해 시간 차이를 계산합니다. 위 예제에서도 두 날짜의 차이를 초단위로 반환합니다.

대체로, 날짜와 시간을 비교하는 데 Java의 `Date` 클래스나 Python의 `datetime` 같은 다른 언어들의 방식과는 많은 차이가 있습니다.

## 참고 자료
Elm에 관한 다른 정보를 찾고 싶다면 아래 링크들을 참고하세요:

1. [Elm 공식 문서](https://guide.elm-lang.jp/)
2. [Elm 시간 관련 패키지](https://package.elm-lang.org/packages/elm/time/latest/)
3. [Elm에 대한 추가 학습 자료](https://www.elm-tutorial.org/en/)
4. [Elm 커뮤니티 포럼](https://discourse.elm-lang.org/)