---
title:                "Haskell: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# 왜 

날짜를 문자열로 변환하는 일을 하는 이유는 무엇일까요? 이 것은 우리가 코드에서 날짜를 표현하고 다른 형식으로 사용하기 때문입니다.

# 어떻게

우리는 Haskell에서 날짜를 문자열로 변환하는 방법에 대해 알아보겠습니다.

```Haskell
import Data.Time.Format
import Data.Time.Calendar

-- 현재 날짜를 가져오기
now <- getCurrentTime

-- 날짜를 원하는 형식으로 변환하기
let formatted = formatTime defaultTimeLocale "%d %B %Y" now 

-- 출력: 03 June 2021
print formatted
```

Haskell의 `formatTime` 함수를 사용하여 `DateTimeLocale`과 원하는 형식 문자열을 지정하여 날짜를 원하는 형식으로 변환할 수 있습니다.

# 깊게 파고들기

날짜를 문자열로 변환하는 것은 날짜와 시간을 처리하는 중요한 작업 중 하나입니다. Haskell에서는 날짜와 시간을 `Data.Time` 모듈을 사용하여 다양한 방식으로 처리할 수 있습니다. 예를 들어, `Day` 데이터 타입을 사용하여 날짜를 표현하고 `parseTimeM` 함수를 사용하여 문자열로부터 날짜를 파싱할 수 있습니다. 또한 다른 데이터 타입인 `UTCTime`, `TimeZone`, `LocalTime` 등도 제공하며 다양한 시간대와 날짜 형식을 처리할 수 있습니다.

# 관련자료

- [Haskell에서 날짜와 시간을 다루는 방법](https://wiki.haskell.org/wikiupload/b/b8/Introduction_Intern_Thesis_Stefan_Lang.pdf)
- [Hackage - Haskell에서 날짜와 시간을 다루는 모듈](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Date & Time manipulation in Haskell blog post](https://www.stackbuilders.com/news/date-time-manipulation-in-haskell/)