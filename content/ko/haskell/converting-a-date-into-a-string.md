---
title:                "날짜를 문자열로 변환하기"
html_title:           "Haskell: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜
날짜를 문자열로 변환하는 작업을 할 때 어떤 이유로 이를 수행하는지 알아볼까요? 날짜를 문자열로 표시하면 화면에 보기 좋게 나타나게 됩니다. 그리고 일반적으로 컴퓨터에서 처리하기 쉬운 형태로 변환될 수 있습니다.

## 어떻게
날짜를 문자열로 변환하는 방법을 살펴보겠습니다. 예를 들어, 날짜를 "YYYY-MM-DD" 형식의 문자열로 표시해 보겠습니다.

```Haskell
import Data.Time.Format
import Data.Time.LocalTime

-- 오늘 날짜를 가져옵니다.
today <- getZonedTime

-- 날짜를 "YYYY-MM-DD" 형식의 문자열로 변환합니다.
let dateString = formatTime defaultTimeLocale "%Y-%m-%d" today

-- 변환된 문자열을 출력합니다.
putStrLn $ "Today's date is: " ++ dateString
```

출력 결과는 다음과 같을 것입니다.

```
Today's date is: 2021-09-08
```

## 깊이 파헤치기
지금까지 우리는 `formatTime` 함수를 사용하여 날짜를 원하는 형식의 문자열로 변환하는 방법을 살펴보았습니다. 이 함수는 `Data.Time.Format` 모듈에서 제공됩니다. 또한 저는 `Data.Time.LocalTime` 모듈에서 `getZonedTime` 함수를 사용하여 오늘 날짜를 가져오는 예시를 보여드렸습니다.

`formatTime` 함수는 2개의 인자를 받습니다. 첫번째 인자는 날짜를 형식화하는 데 사용될 형식입니다. 두번째 인자는 문자열로 변환될 날짜 객체입니다.

더 많은 형식화 옵션 및 관련 함수에 대해 알아보려면 [Haskell 공식 문서](https://hackage.haskell.org/package/time-1.11.1/docs/Data-Time-Format.html#g:9)를 참조하시기 바랍니다.

## 더 알아보기
Haskell에서 현재 날짜와 시간을 다루는 방법에 대해 더 알아보려면 [Haskell에서 시간과 날짜 다루기](https://wiki.haskell.org/Time_and_Date)를 읽어보시기 바랍니다.