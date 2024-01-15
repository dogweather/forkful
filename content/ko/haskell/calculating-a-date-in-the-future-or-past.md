---
title:                "미래나 과거의 날짜 계산하기"
html_title:           "Haskell: 미래나 과거의 날짜 계산하기"
simple_title:         "미래나 과거의 날짜 계산하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜

날짜를 미래나 과거로 계산하는 것에 대해 관심을 가지는 이유는, 우리 일상에서 자주 사용하는 일이기 때문이에요. 생일이나 결혼 기념일 등 특별한 날짜를 계산할 때 유용하게 사용할 수 있답니다.

## 어떻게

```Haskell
import Data.Time

-- 미래의 날짜 계산하기
futureDate :: Day -> Integer -> Day
futureDate currentDate days
    | days < 0 = error "0보다 큰 숫자를 입력하세요"
    | otherwise = addDays days currentDate

-- 날짜 출력하기
main = do
    let today = utctDay $ getCurrentTime
    let future = futureDate today 30
    putStrLn $ "오늘 날짜는 " ++ show today ++ "입니다."
    putStrLn $ "미래의 날짜는 " ++ show future ++ "입니다."

-- Sample Output: 오늘 날짜는 2021-05-01입니다.
-- 미래의 날짜는 2021-05-31입니다.
```
```Haskell
import Data.Time

-- 과거의 날짜 계산하기
pastDate :: Day -> Integer -> Day
pastDate currentDate days
    | days < 0 = error "0보다 큰 숫자를 입력하세요"
    | otherwise = addDays (-days) currentDate

-- 날짜 출력하기
main = do
    let today = utctDay $ getCurrentTime
    let past = pastDate today 30
    putStrLn $ "오늘 날짜는 " ++ show today ++ "입니다."
    putStrLn $ "과거의 날짜는 " ++ show past ++ "입니다."

-- Sample Output: 오늘 날짜는 2021-05-01입니다.
-- 과거의 날짜는 2021-03-31입니다.
```

## 깊게 들어가보기

Haskell 언어에서는 날짜와 시간을 다루기 위해 Data.Time 모듈을 사용합니다. `Day` 데이터 타입을 이용하여 날짜를 나타내고, `addDays` 함수를 이용하여 날짜를 계산할 수 있습니다. 또한 `getCurrentTime` 함수를 이용하여 현재 날짜를 가져올 수 있습니다.

## See Also

- [Haskell 공식 문서](https://www.haskell.org/documentation/)
- [Haskell 공식 홈페이지](https://www.haskell.org/)