---
title:                "Haskell: 현재 날짜 가져오기"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 왜

현재 날짜를 얻는 과정에 관심을 가지는 이유는 다양합니다. 일상적인 프로그래밍 작업에서 날짜 정보를 사용하기 위해서, 또는 날짜와 관련된 여러 작업들을 자동화하기 위해서 일 수 있습니다. 바로 어떤 이유로든, Haskell을 사용하여 현재 날짜를 얻는 방법을 배울 것입니다.

## 어떻게

우선, `Data.Time` 모듈을 가져와서 날짜와 시간 정보를 다룰 수 있도록 지원 받아야 합니다.

```Haskell
import Data.Time
```

그 다음, `getCurrentTime` 함수를 이용하여 현재 시간을 가져올 수 있습니다. 이 함수는 `IO UTCTime` 타입을 반환하므로, `do` 블록 안에서 패턴 매칭을 이용하여 값을 바인딩해줄 수 있습니다.

```Haskell
do
  currentTime <- getCurrentTime
```

이제 날짜와 시간 정보를 원하는 형식으로 변환하기 위해 `Data.Time.Format` 모듈을 가져와야 합니다. 예를 들어, 현재 날짜를 `"yyyy-mm-dd"` 형식의 문자열로 만들어보겠습니다.

```Haskell
import Data.Time.Format

do
  currentTime <- getCurrentTime
  let dateString = formatTime defaultTimeLocale "%Y-%m-%d" currentTime
```

최종적으로, `dateString` 변수에는 현재 날짜를 나타내는 문자열이 담겨 있게 됩니다. 아래는 실제 실행 결과 예시입니다.

```
2021-06-22
```

## 깊게 파보기

현재 날짜를 얻는 과정에서 중요한 개념은 `UTCTime`과 `ZonedTime`으로 현재 시간을 나타낼 수 있다는 것입니다. `UTCTime`은 세계 표준시(UTC)에 따른 현재 시간을 나타내는 타입이고, `ZonedTime`은 특정 시간대에 따른 현재 시간을 나타내는 타입입니다.

또 다른 중요한 개념은 시간과 관련된 포맷팅입니다. `formatTime` 함수에서 이용한 `defaultTimeLocale`은 기본적으로 영어로 출력하는 포맷팅 규칙을 가지고 있지만, 한국어로 출력하고 싶다면 `koreanTimeLocale`과 같은 사용자 정의 포맷팅을 이용할 수도 있습니다.

# 더 읽어보기

- [Haskell 공식 문서 - Getting the current date and time](https://www.haskell.org/haskellwiki/Getting_the_current_date_and_time)
- [Real World Haskell - Dealing with Time and Date](http://book.realworldhaskell.org/read/using-time-and-date.html)