---
title:    "Elm: 현재 날짜 가져오기"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜

현재 날짜를 알아내는 것은 Elm 프로그래밍에서 매우 일반적입니다. 이로 인해 사용자 인터페이스에서 현재 날짜를 표시하거나 날짜와 관련된 기능을 구현하는 데 유용합니다.

## 사용 방법

```Elm
import Time

-- 현재 날짜를 가져오는 함수
getTime : Task x Time.Posix 
getTime =
  Time.now

-- 현재 날짜를 표시하는 예제
main =
  Html.text "오늘의 날짜는 " ++ (Time.millisToUtc 0 |> Time.toYearMonthDay)
```

위의 코드를 실행하면 현재 시간을 밀리 초로 반환하는 `getTime` 함수와 `main` 함수를 볼 수 있습니다. `main` 함수에서는 현재 날짜를 표시하기 위해 `Time.millisToUtc` 함수와 `Time.toYearMonthDay` 함수를 사용합니다.

실행하면 다음과 같은 결과가 나옵니다.

```Elm
"오늘의 날짜는 (2021, 9, 20)"
```

위의 코드를 변경하여 현재 시간을 더 다양한 방법으로 표현할 수도 있습니다. 예를 들어 `Time.toHourMinuteSecond` 함수를 사용하여 시, 분, 초를 포함한 시간을 알 수 있습니다.

## 깊이있는 알아보기

현재 날짜를 알아내는 방법은 Elm에서 매우 쉽습니다. 이유는 바로 `Time` 모듈을 통해 제공되기 때문입니다. `Time` 모듈은 시간과 날짜를 표현하기 위한 다양한 함수들을 제공합니다. 이를 사용하여 현재 날짜를 표현하는 것 외에도, 지난 날짜 또는 미래 날짜를 계산하는 것도 가능합니다.

더 깊이있는 내용을 알고 싶다면 [Elm 공식 문서](https://package.elm-lang.org/packages/elm/time/latest/Time)를 참조하십시오.

## 또 다른 참고자료

- [Elm 입문서](https://elm-programming.netlify.app/chapter03/03_01.html)
- [Elm 공식 문서](https://guide.elm-lang.org/)
- [Elm 한국어 커뮤니티](https://www.facebook.com/groups/ElmKR)