---
title:    "Elm: 미래나 과거의 날짜 계산하기"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜

날짜를 미래나 과거로 계산하는 것은 우리 삶에서 매우 중요한 부분입니다. 우리가 예를 들어 약속을 잡을 때나 일정을 관리할 때 날짜를 계산하는 데에는 많은 시간과 노력이 듭니다. 그렇기 때문에, 이 작업을 자동화해주는 Elm 프로그래밍 언어가 우리에게 매우 유용합니다.

## 어떻게

```Elm
-- 현재 날짜를 가져오는 함수
currentDate : Date
currentDate =
    let
        timeZone =
            Date.utc
    in
    Date.fromTime (Time.posixToMillis 0) timeZone
    
-- 미래나 과거로 날짜를 계산하는 함수
calculateDate : Int -> Date -> Date
calculateDate duration date =
    let
        time =
            Time.millisToPosix (Date.toTime date)
    in
    let
        newTime =
            time + (Time.millis duration)
    in
    let
        timeZone =
            Date.utc
    in
    Date.fromTime newTime timeZone
    
-- 1년 후의 날짜 계산 예제
oneYearLater =
    calculateDate (365 * Time.inMilliseconds Time.day) currentDate

-- 5일 전의 날짜 계산 예제
fiveDaysAgo =
    calculateDate (5 * Time.inMilliseconds Time.day) currentDate
```

위의 코드를 실행하면 `oneYearLater`와 `fiveDaysAgo`에 각각 1년 후와 5일 전의 날짜가 계산되어 할당됩니다.

## 깊게 들어가기

날짜 계산을 할 때에는 주의해야 할 몇 가지 사항이 있습니다. Elm의 날짜와 시간 라이브러리는 타임존을 고려하여 작동하기 때문에, 계산할 때 타임존을 반드시 지정해주어야 합니다. 또한 윤년이나 서머타임과 같은 특수한 경우에도 염두해두어야 합니다.

## 더 알아보기

- [Elm 공식 문서](https://guide.elm-lang.org/dates_and_times.html)
- [Elm에서 날짜와 시간 다루기](https://medium.com/witinui/%EB%B2%88%EC%97%AD-elm-next-js%EB%A1%9C-%EB%82%A0%EC%A7%9C%EC%99%80-%EC%8B%9C%EA%B0%84-%EB%8B%A4%EB%A3%A8%EA%B8%B0-fe2e6d540ec)
- [Elm으로 날짜와 시간 다루기](https://medium.com/@Cryptovexillology/elm-%EB%82%A0%EC%A7%9C%EC%99%80-%EC%8B%9C%EA%B0%84-%EB%8B%A4%EB%A3%A8%EA%B8%B0-aafe49df4a6d)

## 관련 링크

- [Elm으로 간단한 ToDo 앱 만들어보기](https://koreanblog.dev/elm%EC%9C%BC%EB%A1%9C-%ED%8A%B9%EC%95%88-%EB%A7%8C%EB%93%A4%EC%96%B4%EB%B3%B4%EA%B8%B0-%EA%B0%84%EB%8B%A8%ED%95%9C-todo-%EC%95%B1-%EB%A7%8C%EB%93%A4%EA%B8%B0/)
- [Elm 새로운 특징 알아보기](https://athinking.dev/blog/feature-overview-elm/?utm_source=tistory)
- [Elm으로 새로운 프로젝트 시작하기](https://wonny.dev/blog/post/a_new_project_with_elm/)