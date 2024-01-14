---
title:                "Haskell: 미래 또는 과거의 날짜 계산하기"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

##왜
언제 일이 발생할 지 모를 때, 우리는 날짜를 미리 계산하고 싶을 수 있습니다. 그에 대한 근거가 어디에 있어야한다는 물음을 해결할 수 있기 때문입니다.

##어떻게
이 글에서는 히스켈 언어로 당신의 생각을 구현하는 방법에 대해 알아보겠습니다. 또한 과거나 미래의 특정한 날짜를 계산하고 출력하는 예제 코드를 제공할 것입니다.

```Haskell
-- 현재 날짜를 얻는 함수
getCurrentDate :: IO Day

-- 미래 날짜 계산하는 함수
-- 예를 들어, 5일 후의 날짜를 계산
addDays :: Integer -> Day -> Day
addDays 5 currentDay
  -- 결과: 2020-10-05

main = do
  -- 현재 날짜 얻기
  currentDay <- getCurrentDate
  -- 5일 후의 날짜 계산하기
  let futureDay = addDays 5 currentDay
  -- 출력
  putStrLn ("5일 후의 날짜는 " ++ show futureDay ++" 입니다.")
```

출력: `5일 후의 날짜는 2020-10-05 입니다.`

##딥 다이브
히스켈에서는 `Data.Time` 라이브러리를 사용하여 날짜와 시간을 다룰 수 있습니다. 이 라이브러리에는 날짜 계산을 위한 여러 가지 함수들이 있습니다. 예를 들어서 `addDays` 함수를 이용하면 특정한 날짜에 일정한 일 수를 더한 결과를 얻을 수 있습니다. 이를 활용하여 프로그램을 작성하면 미래의 특정한 날짜를 간편하게 계산하고 출력할 수 있습니다.

##참고
- [Haskell 공식 문서 - Data.Time](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/time-1.9.3/Data-Time.html)
- [히스켈로 캘린더 만들기](https://typeable.io/posts/2019-02-24-easy-cal.html)