---
title:                "날짜를 문자열로 변환하는 방법"
html_title:           "Elm: 날짜를 문자열로 변환하는 방법"
simple_title:         "날짜를 문자열로 변환하는 방법"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

날짜를 문자열로 변환하는 것은 날짜를 컴퓨터가 이해할 수 있는 형식으로 변경하는 것을 말합니다. 프로그래머들은 이 작업을 하는 이유는 컴퓨터에서 날짜를 다룰 때 편리하게 하기 위해서입니다.

## 어떻게:

```Elm 
import Time
import Date

Date.toString (Date.fromMillis 1583358000000)

--> "2020-03-05"
```

위의 코드는 날짜를 문자열로 변환하는 가장 간단한 방법입니다. ```Date.toString``` 함수를 사용하여 날짜를 문자열로 변환할 수 있습니다.

```Elm 
Time.millisToPosix (Time.millisSinceEpoch 1583358000000)

--> { millis = 1583358000000, posix = 2020-03-05T00:00:00Z }
```

또 다른 방법은 ```Time.millisSinceEpoch``` 함수를 사용하여 날짜와 시간을 밀리초로 변환하고, ```Time.millisToPosix``` 함수를 사용하여 날짜를 문자열로 변환하는 것입니다. 이 방법은 밀리초를 다룰 수 있는 더 많은 유연성을 제공합니다.

## 깊게 파보기:

이전에는 날짜를 문자열로 변환하기 위해 많은 노력이 필요했습니다. 그러나 Elm에서는 내장 함수를 사용하여 간단하게 이 작업을 수행할 수 있습니다. 또한 이전에는 서로 다른 시간대를 다루기가 어려웠지만, 우리는 이제 나라별 시간대를 자동으로 고려하는 내장 함수를 제공합니다.

## 참고:

- Elm 공식 문서(https://elm-lang.org/docs)
- 시간 라이브러리 관련 문서(https://package.elm-lang.org/packages/elm/time/latest/)