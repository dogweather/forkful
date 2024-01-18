---
title:                "문자열에서 날짜 구문 분석하기"
html_title:           "Haskell: 문자열에서 날짜 구문 분석하기"
simple_title:         "문자열에서 날짜 구문 분석하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 무엇이고 왜?

날짜를 문자열에서 추출하기는 프로그래머들이 자주 하는 일입니다. 이것은 개발자가 사용자로부터 날짜 형식을 문자열로 입력받은 경우, 정보를 추출하고 이를 다른 형식으로 변경하기 위해 필요합니다.

## 어떻게:

Haskell에서 날짜를 문자열에서 추출하는 방법을 살펴보겠습니다. 아래의 코드 예제를 보면서 동작 방식을 이해해보세요.

```Haskell
module Main where
import Data.Time.Format
import Data.Time.Clock
main :: IO ()
main = do
  -- 문자열로 표현된 날짜
  let dateStr = "2021-01-01"
  -- 날짜 형식을 지정 (ISO-8601)
  let dateFormat = iso8601DateFormat (Just "%H:%M:%S")
  -- 문자열에서 날짜 추출
  let parsedDate = parseTimeOrError True dateFormat dateStr :: UTCTime
  -- 추출된 날짜 출력
  putStrLn (show parsedDate)
```

위의 코드를 실행하면 "2021-01-01 00:00:00 UTC"라는 결과가 출력됩니다.

## 깊이 들어가기:

날짜를 문자열에서 추출하는 것은 매우 유용한 기능이지만, 이전에는 복잡한 작업이었습니다. 하지만 Haskell에서는 Data.Time.Format 모듈을 사용하여 쉽게 구현할 수 있습니다. 또한, 여러 형식을 지원하므로 자신에게 적합한 날짜 형식을 선택할 수 있습니다. 또한 라이브러리를 통해 다양한 날짜 연산도 가능합니다.

## 관련 자료:

- https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html
- https://www.tutorialspoint.com/haskell/haskell_date_time.htm