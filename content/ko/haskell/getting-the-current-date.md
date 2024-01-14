---
title:    "Haskell: 현재 날짜 가져오기"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜

현재 날짜를 가져오는 것은 많은 프로그래밍 언어에서 유용한 기능입니다. 이 기능을 사용하면 현재 날짜를 쉽게 확인할 수 있으며, 이를 바탕으로 다양한 작업을 수행할 수 있습니다.

## 사용 방법

먼저 Haskell에서 현재 날짜를 가져오는 방법을 살펴보겠습니다. 아래의 코드를 사용하면 현재 날짜를 가져올 수 있습니다.

```Haskell
import Data.Time
getCurrentTime
```

위의 코드를 실행하면 현재 날짜와 시간이 출력됩니다. 예를 들어, "2021-06-26 11:30:00.123456789 UTC"와 같은 형식으로 출력될 것입니다.

## 깊게 파헤치기

더 자세한 내용을 알고 싶다면, `Data.Time` 모듈을 살펴볼 수 있습니다. 이 모듈은 날짜와 시간을 다루는 다양한 함수들을 제공합니다. 예를 들어, `localDay` 함수를 사용하면 현재 날짜를 지역 시간대에 맞춰 출력할 수 있습니다.

더 나아가 `getCurrentTime` 함수는 `IO UTCTime` 형식을 반환합니다. 따라서 이를 사용하기 위해서는 `IO` 모네드를 이용해야 합니다. `monad`라는 개념은 함수형 프로그래밍에서 중요한 개념이므로, 함수와 함께 유연하게 사용할 수 있도록 공부해두는 것이 좋습니다.

## See Also

- [Haskell Wiki](https://wiki.haskell.org/Handling_time_zone_in_Haskell)
- [A Gentle Introduction to Haskell](https://www.haskell.org/tutorial/time.html)
- [Learn You a Haskell](http://learnyouahaskell.com/input-and-output)