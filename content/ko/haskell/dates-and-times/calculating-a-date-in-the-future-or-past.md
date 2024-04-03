---
date: 2024-01-20 17:31:11.654524-07:00
description: "\uBBF8\uB798\uB098 \uACFC\uAC70\uC758 \uB0A0\uC9DC\uB97C \uACC4\uC0B0\
  \uD558\uB294 \uAC83\uC740 \uD2B9\uC815 \uAE30\uC900\uC77C\uB85C\uBD80\uD130 \uD2B9\
  \uC815 \uAE30\uAC04\uC744 \uB354\uD558\uAC70\uB098 \uBE7C\uC11C \uB0A0\uC9DC\uB97C\
  \ \uC5BB\uB294 \uACFC\uC815\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\
  \uC740 \uC720\uD6A8\uAE30\uAC04 \uCC98\uB9AC, \uC608\uC57D \uC2DC\uC2A4\uD15C, \uB370\
  \uC774\uD130 \uBD84\uC11D \uB4F1\uC758 \uB2E4\uC591\uD55C \uC0C1\uD669\uC5D0\uC11C\
  \ \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.314930-06:00'
model: gpt-4-1106-preview
summary: "\uBBF8\uB798\uB098 \uACFC\uAC70\uC758 \uB0A0\uC9DC\uB97C \uACC4\uC0B0\uD558\
  \uB294 \uAC83\uC740 \uD2B9\uC815 \uAE30\uC900\uC77C\uB85C\uBD80\uD130 \uD2B9\uC815\
  \ \uAE30\uAC04\uC744 \uB354\uD558\uAC70\uB098 \uBE7C\uC11C \uB0A0\uC9DC\uB97C \uC5BB\
  \uB294 \uACFC\uC815\uC785\uB2C8\uB2E4."
title: "\uBBF8\uB798\uB098 \uACFC\uAC70\uC758 \uB0A0\uC9DC \uACC4\uC0B0\uD558\uAE30"
weight: 26
---

## 무엇 & 왜?

미래나 과거의 날짜를 계산하는 것은 특정 기준일로부터 특정 기간을 더하거나 빼서 날짜를 얻는 과정입니다. 프로그래머들은 유효기간 처리, 예약 시스템, 데이터 분석 등의 다양한 상황에서 사용합니다.

## 사용 방법:

```Haskell
import Data.Time

-- 기준 날짜를 정하고, 동일한 날짜 객체 형식으로 반환
let baseDate = fromGregorian 2023 3 14 -- 2023년 3월 14일

-- 날짜에 일수를 더하거나 빼기
let tenDaysLater = addDays 10 baseDate
let tenDaysBefore = addDays (-10) baseDate

-- 결과 출력
main = do
  putStrLn $ "Ten days later: " ++ show tenDaysLater
  putStrLn $ "Ten days before: " ++ show tenDaysBefore
```

출력:
```
Ten days later: 2023-03-24
Ten days before: 2023-03-04
```

## 깊이 있는 정보:

날짜 계산은 그리고리력(Gregorian calendar)을 기반으로 합니다. 그리고리력은 1582년에 도입되어 오늘날 대부분 국가에서 사용됩니다. Haskell의 `Data.Time` 라이브러리는 이력 관리와 관련된 강력한 기능을 제공합니다.

대안으로, 라이브러리 사용 없이 순수한 함수로 날짜를 직접 계산하는 방법도 있지만 복잡한 달력 규칙과 윤년 처리를 고려해야 합니다.

데이터 타입과 다양한 날짜 및 시간 함수들(`addDays`, `addGregorianMonthsRollOver` 등)을 사용하여 원하는 날짜 산출은 상대적으로 간단합니다. Haskell의 타입 시스템은 오류를 줄이고 명확한 코드 작성을 돕습니다.

## 참고 자료:

- Haskell Documentation: [Data.Time](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- [Zvon Haskell Reference: The Calendar and Time Library](http://zvon.org/other/haskell/Outputglobal/index.html)

이 문서들은 Haskell의 날짜 및 시간에 관련된 기능을 더 자세히 이해하는데 도움을 줄 것입니다.
