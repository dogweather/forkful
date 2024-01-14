---
title:    "Haskell: 현재 날짜 가져오기"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## 왜

현재 날짜를 얻는 것은 많은 프로그래밍 언어에서 자주 사용되는 기능입니다. 서버의 로그를 기록할 때나 사용자의 생일을 저장할 때 등 다양한 상황에서 현재 날짜를 필요로 할 수 있습니다.

## 어떻게

Haskell은 일반적인 프로그래밍 언어와는 조금 다르게 현재 날짜를 가져오는 방법이 있습니다. 먼저 `Data.Time` 라이브러리를 임포트해야 합니다.

``` haskell
import Data.Time
```

그리고 `getCurrentTime` 함수를 사용하여 날짜와 시간을 얻을 수 있습니다.

``` haskell
getCurrentTime :: IO UTCTime
```

그리고 `formatTime` 함수를 사용하여 원하는 형식으로 날짜와 시간을 포맷할 수 있습니다. 아래는 예시 코드와 그 결과입니다.

``` haskell
main :: IO ()
main = do
  now <- getCurrentTime
  let formattedDate = formatTime defaultTimeLocale "%Y년 %m월 %d일" now
  putStrLn formattedDate
```

```
2019년 10월 22일
```

## 딥 다이브

Haskell에서 날짜와 시간을 다루는 방법에 대해 더 깊이 알아보겠습니다. `Data.Time` 라이브러리에는 날짜와 시간을 다루는 다양한 유형들이 정의되어 있습니다. 가장 기본적인 유형은 `UTCTime`인데, 협정 세계시(UTC)를 기준으로 한 날짜와 시간을 나타냅니다.

`formatTime` 함수에서 우리는 날짜와 시간을 특정 형식으로 포맷할 수 있었습니다. 이때 사용한 `defaultTimeLocale`은 기본적인 형식 설정을 제공하는 로케일(지역)입니다. 만약 원하는 형식이 없을 경우 직접 로케일을 정의할 수도 있습니다.

## 해당 문서와 관련된 링크

- [Haskell 공식 문서 - Data.Time 라이브러리](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- [Haskell 공식 문서 - 시간과 날짜 다루기](https://www.haskell.org/haskellwiki/Practical_Haskell/Dates_and_times)
- [Haskell 로컬 및 시간 설정하기](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/Simple%20date%20and%20time%20types)
- [Haskell로 날짜와 시간 다루기](http://unindex.hatenablog.com/entry/20111204/1322978441)