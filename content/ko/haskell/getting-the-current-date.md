---
title:                "현재 날짜 얻기"
html_title:           "Haskell: 현재 날짜 얻기"
simple_title:         "현재 날짜 얻기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜

현재 날짜를 얻는 것의 이점은 다음과 같습니다:
- 일자 기반 작업을 자동화하기 위해 (예: 일일 보고서 생성)
- 어떤 일이 발생한 시간을 기록하기 위해 (예: 사용자의 로그인 시간 기록)

## 방법

우선 `getCurrentTime` 함수를 사용하여 현재 날짜와 시간을 얻을 수 있습니다. 이 함수는 `IO` 타입이기 때문에, `do` 표현식을 사용하여 값을 받아와서 출력할 수 있습니다.

```Haskell
import Data.Time

main = do
  currentTime <- getCurrentTime
  print currentTime
```

출력 예시:

```
2020-05-25 18:00:00.123456789 UTC
```

`getCurrentTime`으로 얻은 값은 `UTCTime` 타입입니다. `UTCTime`에는 날짜와 시간 뿐만 아니라 타임존 정보까지 포함되어 있습니다. 따라서 원하는 형식으로 날짜와 시간을 출력하기 위해서는 `Data.Time.Format` 모듈에서 제공하는 함수를 사용해야 합니다.

아래의 예시 코드는 `Data.Time.Format`을 사용하여 날짜와 시간을 Hour-Minute-Second의 형식으로 출력하는 방법을 보여줍니다.

```Haskell
import Data.Time
import Data.Time.Format

main = do
  currentTime <- getCurrentTime
  let formattedTime = formatTime defaultTimeLocale "%H:%M:%S" currentTime
  print formattedTime
```

출력 예시:

```
18:00:00
```

## 딥 다이브

`getCurrentTime`을 사용하여 얻은 `UTCTime` 값에는 어떤 정보가 포함되어 있을까요? 각각의 정보를 다음과 같이 간단히 살펴보도록 하겠습니다.

```Haskell
data UTCTime = UTCTime {
   utctDay :: Day,
   utctDayTime :: DiffTime
}
```

- `utctDay`: 해당 날짜의 `Day` 타입 값을 나타냅니다. `Day` 타입은 파싱이나 비교 연산을 할 수 있도록 년, 월, 일 등의 정보를 담고 있습니다.
- `utctDayTime`: 해당 날짜의 시간 정보를 나타내는 `DiffTime` 값입니다. `DiffTime`은 일(day) 단위가 아닌 초 단위로 시간을 나타내는 타입입니다.

위에서 사용한 `getCurrentTime`은 `System.Clock` 모듈에서 제공하는 함수 중 하나입니다. 이 외에도 `System.Clock`에서 시간과 관련된 유용한 함수들을 제공하고 있으니 필요하다면 참고하시기 바랍니다.

## See Also

- [Haskell Docs - Data.Time](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Haskell Docs - System.Clock](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-Clock.html)