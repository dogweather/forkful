---
title:    "Haskell: 두 날짜 비교하기"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜

날짜 비교를 하는 이유는, 프로그래밍에서 일상적으로 발생하는 작업이기 때문입니다. 날짜를 비교하는 것은 예약 시스템에서 날짜를 확인하거나, 이벤트의 시작일과 종료일을 비교할 때 많이 사용됩니다.

## 어떻게

날짜 비교를 하기 위해서는 기본적으로 `Data.Time` 모듈을 사용해야 합니다. 해당 모듈은 날짜와 시간을 다루는 데 필요한 여러 함수와 데이터 타입을 제공합니다. 아래는 두 날짜를 비교하는 예제 코드입니다.

```Haskell
import Data.Time

let date1 = fromGregorian 2020 01 01
let date2 = fromGregorian 2020 01 03

print (date1 < date2) -- Output: True
print (date1 == date2) -- Output: False
```

비교 연산자 `<` 는 첫 번째 날짜가 두 번째 날짜보다 이전인지를 확인하고, `==` 는 두 날짜가 동일한지를 확인합니다.

## 딥 다이브

날짜 비교는 특히 윤년과 같은 특별한 경우에 주의해야 합니다. 예를 들어, `fromGregorian` 함수는 4년마다 하루를 추가하지만 100년마다는 추가하지 않습니다. 하지만 400년마다는 다시 하루를 추가합니다. 따라서 윤년으로 판단되는 해가 4의 배수이지만 100의 배수인 경우에는 하루를 추가하지 않는 것을 알아야 합니다.

또한, 날짜와 시간을 다루는 다른 모듈들도 많이 있으니 필요에 따라 적절하게 사용하는 것이 좋습니다.

## 그 밖에 살펴보기

- [Hackage: Data.Time 모듈](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Learn You a Haskell for Great Good!: Dates and Times](http://learnyouahaskell.com/types-and-typeclasses#dates-and-times)
- [Real World Haskell: Dates and Times](http://book.realworldhaskell.org/read/using-parsec.html#io.parsingControl.MonadPlus)
- [Haskell Wiki: 날짜와 시간](https://wiki.haskell.org/Date_and_time_library)
- [Haskell Korea: 모나드가 하는 일 해설](https://haskellkorea.github.io/posts/2011-08-23-monads-do-not-work-like-you-think)