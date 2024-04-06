---
date: 2024-01-20 17:31:11.654524-07:00
description: "\uC0AC\uC6A9 \uBC29\uBC95: \uB0A0\uC9DC \uACC4\uC0B0\uC740 \uADF8\uB9AC\
  \uACE0\uB9AC\uB825(Gregorian calendar)\uC744 \uAE30\uBC18\uC73C\uB85C \uD569\uB2C8\
  \uB2E4. \uADF8\uB9AC\uACE0\uB9AC\uB825\uC740 1582\uB144\uC5D0 \uB3C4\uC785\uB418\
  \uC5B4 \uC624\uB298\uB0A0 \uB300\uBD80\uBD84 \uAD6D\uAC00\uC5D0\uC11C \uC0AC\uC6A9\
  \uB429\uB2C8\uB2E4. Haskell\uC758 `Data.Time` \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB294\
  \ \uC774\uB825 \uAD00\uB9AC\uC640 \uAD00\uB828\uB41C \uAC15\uB825\uD55C \uAE30\uB2A5\
  \uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4. \uB300\uC548\uC73C\uB85C,\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.024892-06:00'
model: gpt-4-1106-preview
summary: "\uB0A0\uC9DC \uACC4\uC0B0\uC740 \uADF8\uB9AC\uACE0\uB9AC\uB825(Gregorian\
  \ calendar)\uC744 \uAE30\uBC18\uC73C\uB85C \uD569\uB2C8\uB2E4."
title: "\uBBF8\uB798\uB098 \uACFC\uAC70\uC758 \uB0A0\uC9DC \uACC4\uC0B0\uD558\uAE30"
weight: 26
---

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
