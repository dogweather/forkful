---
title:                "Haskell: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜
현재 날짜를 가져오는 것을 왜 하는지 궁금하신가요? 프로그래밍을 하는 데 있어서 많은 경우 현재 날짜와 시간을 사용해야 할 때가 있습니다. 예를 들어, 파일 또는 데이터베이스에 로그를 남기거나, 알림을 표시하거나, 배송일자를 계산할 때 등이 있습니다.

## 어떻게
Haskell 언어를 사용하여 현재 날짜를 가져오는 것은 간단합니다. 아래의 코드 예제를 확인해보세요.

```Haskell
import Data.Time.Clock.LocalTime
import Data.Time.Calendar

-- 오늘 날짜와 시간을 가져오기 위해 getCurrentTime 함수를 사용합니다.
getCurrentTime :: IO UTCTime

-- UTCTime에서 LocalTime으로 변환합니다.
utcToLocalTime :: TimeZone -> UTCTime -> LocalTime

-- UTC 날짜를 스트링 형식으로 변환합니다.
show :: FormatTime t => t -> String

-- 오늘 날짜와 시간을 출력합니다.
main = do
    time <- getCurrentTime
    let localTime = utcToLocalTime (minutesToTimeZone 540) time
    let date = show localTime
    putStrLn date
```
실행 결과:
```
2021-11-29 12:30:45
```

## 심층 분석
보다 정확한 날짜와 시간을 얻기 위해서는 `TimeZone` 모듈을 불러와 TimeZone 데이터 형식을 이용해야 합니다. 이 모듈은 지역 시간과 UTC 사이의 차이를 계산할 수 있는 함수들을 제공합니다. 입력된 `TimeZone` 값은 분 단위로 설정되는데, 한국 기준의 GMT+09:00과 같은 값이 됩니다. 이 외에도 `getCurrentTime` 함수를 이용해 년, 월, 일, 시간 등 다양한 정보를 얻을 수 있습니다.

## 참고 자료
- [Haskell의 Data.Time.Clock.LocalTime 모듈](https://hackage.haskell.org/package/time-locale-compat-0.1.1/docs/Data-Time-Clock-LocalTime.html)
- [Haskell의 Data.Time.Calendar 모듈](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Calendar.html)
- [Haskell의 FormatTime 함수](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html#v:formatTime)