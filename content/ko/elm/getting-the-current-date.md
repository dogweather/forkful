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

## 왜

잘봐, 우리가 하루 중 언제든 현재 날짜를 알아내는 것이 유용할 수 있다는 걸 빼놓으면 안 되겠다. 우리는 자주 날짜를 알아내야 할 경우가 있는데, 예를 들면 미팅이나 이벤트 날짜를 정할 때가 있다. Elm을 사용하면 쉽게 현재 날짜를 얻을 수 있다는 것을 알아두자.

## 하우 투

우선, currentDate 변수를 선언하고 어떤 타입의 데이터를 넣을지 지정하자. 예를 들면 다음과 같다.

```Elm
currentDate : Date
currentDate = 
```

그리고 ```getCurrentTime``` 함수를 사용해 현재 날짜를 구해보자. 이 함수는 반환 타입으로 [Time](https://package.elm-lang.org/packages/elm/time/latest/Time) 패키지의 [Posix](https://package.elm-lang.org/packages/elm/time/latest/Time#Posix)를 사용한다. 그래서 우리는 기본적으로 Posix로 변환해야 한다. 여기서는 [Date 간단히 구현하기](https://package.elm-lang.org/packages/elm/core/latest/Date) 에서 제공하는 함수를 사용할 것이다.

```Elm
currentDate : Date
currentDate = 
    Time.toDate << Time.getCurrentTime
```

하지만, 만약 우리가 특정 지역의 시간대를 사용하는 것이 좋다고 생각한다면, [Time.Zone](https://package.elm-lang.org/packages/elm/time/latest/Time-Zone) 모듈을 사용할 수 있다. 예를 들면 다음과 같다.

```Elm
currentDate : Date
currentDate = 
    Time.toDate <|
        Time.getCurrentTime Time.utc

currentDateInSeoul : Date
currentDateInSeoul = 
    Time.toDate <|
        Time.getCurrentTime Time.jst
```

그리고 이제 currentDate 변수를 사용해 정상적으로 현재 날짜를 출력할 수 있다.

```Elm
currentDate : Date
currentDate = 
    Time.toDate << Time.getCurrentTime

main : Html msg
main =
    Html.text <| Date.toIsoString <| currentDate
```

출력 결과는 아마 ```2021-05-26T13:00:00.000Z```와 비슷할 것이다.

## 딥 다이브

더 공부해볼 만한 주제로는 [Chrono](https://package.elm-lang.org/packages/noahzgordon/elm-chrono/latest/) 패키지를 사용해 다양한 날짜와 시간 포맷을 다루는 것이 있다. 또한, Elm 커뮤니티에서는 날짜와 시간을 다루는 더 많은 패키지를 제공하고 있으니 참조하면 도움이 될 것이다.

## 참고

- [Time 패키지 문서](https://package.elm-lang.org/packages/elm/time/latest/Time)
- [Elm 커뮤니티에서 제공하는 날짜와 시간 관련 패키지 목록](https://package.elm-lang.org/search?q=time&platform=all&licenses=license)