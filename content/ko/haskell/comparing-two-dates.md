---
title:                "두 날짜 비교하기"
html_title:           "C#: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가?

두 날짜를 비교한다는 것은 날짜 간의 전후 관계를 파악하거나 사이에 경과한 날짜 수를 계산하는 것입니다. 이를 통해 응용프로그램에서 예약, 스케쥴링, 경과 시간 추적 등에 사용됩니다.

##이렇게 해보세요:

Haskell에서 `Data.Time.Calendar` 모듈을 이용하여 두 날짜를 비교할 수 있습니다. 두 날짜를 비교해보는 코드를 보겠습니다.

```Haskell
import Data.Time.Calendar

date1 = fromGregorian 2022 8 11
date2 = fromGregorian 2022 10 14

main :: IO ()
main = do 
  print $ diffDays date2 date1
```

예를 들어 위 프로그램은 2022년 8월 11일과 2022년 10월 14일 사이의 날짜 차이를 계산하고 출력합니다.

## 디테일하게 살펴보기

두 날짜를 비교하는 것은 프로그래밍의 초창기부터 있었고, 이는 예약 시스템, 타임라인 추적 등 많은 분야에서 필수적인 작업입니다. Haskell에서는 `Data.Time.Calendar` 모듈을 사용해 이것을 수행합니다. 다양한 연산등을 제공하며, 표준 라이브러리로 제공되므로 별도의 설치가 필요하지 않습니다.

Haskell 대신 다른 언어를 사용해야 하는 경우, 대부분의 현대 프로그래밍 언어는 날짜를 비교하는 기능을 제공합니다. 예를 들어 Java에는 `LocalDate` 클래스가 있고, Python에서는 `datetime` 라이브러리를 사용할 수 있습니다.

Haskell의 `diffDays` 함수는 두 날짜를 비교하여 그 차이를 일 단위로 반환합니다. 이 함수는 간단하게 `Day -> Day -> Integer` 타입으로 구현되어 있습니다.

## 참고하면 좋은 링크들

* [Haskell Data.Time 패키지](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
* [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/chapters)
* [Haskell Data.Time.Calendar 디테일 정보(GitHub)](https://github.com/haskell/time)