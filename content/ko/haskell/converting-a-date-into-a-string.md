---
title:    "Haskell: 날짜를 문자열로 변환하기"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜

날짜를 문자열로 변환하는 것이 왜 중요한지 아십니까? 날짜를 다른 프로그래밍 작업에 필요한 형식으로 변환하는 것은 중요한 작업입니다. 예를 들어, 해당 날짜를 파일 이름으로 사용하거나, 데이터베이스에 저장하거나, 사용자가 이해할 수 있는 형식으로 출력하는 등 다양한 용도로 사용됩니다. 이를 위해 Haskell에서는 날짜를 문자열로 변환하는 유용한 함수들을 제공합니다.

## 어떻게

Haskell에서 날짜를 문자열로 변환하는 방법은 간단합니다. 다음의 코드를 따라해보세요.

```Haskell
import Data.Time.Format
import Data.Time.Clock

currentTime <- getCurrentTime
let formattedDate = formatTime defaultTimeLocale "%Y년 %m월 %d일" currentTime
putStrLn formattedDate
```

위 코드는 현재 시간을 받아와서 `%Y년 %m월 %d일` 형식으로 변환해주는 코드입니다. `%Y`, `%m`, `%d`는 각각 연도, 월, 일을 나타냅니다. 이를 원하는 형식으로 바꿔서 사용하면 됩니다. 또 다른 예시로는 `%H시 %M분`과 같이 시간을 나타내는 `%H`와 `%M`도 있습니다.

출력 결과는 다음과 같을 것입니다.

```Haskell
2019년 01월 01일
```

## 깊게 파보기

Haskell에서 날짜를 문자열로 변환할 때, `formatTime` 함수를 사용합니다. 이 함수는 시간 형식 지정자(`%Y`, `%m`, `%d` 등)와 현재 시간을 받아서 지정한 형식으로 변환해줍니다. `%`로 시작하는 문자열은 지정자로, 다른 문자열은 해당 문자열 그대로 출력됩니다.

더 자세한 정보를 알고 싶다면, [Haskell 공식 문서](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/time-1.9.3/Data-Time-Format.html#v:formatTime)를 참조하세요.

## 참고

- [Haskell 날짜 처리 관련 라이브러리](https://hackage.haskell.org/package/time)
- [날짜 형식 지정자 문서](https://www.gnu.org/software/libc/manual/html_node/Locales-and-the-Strftime-Function.html)