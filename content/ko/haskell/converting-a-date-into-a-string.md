---
title:                "날짜를 문자열로 변환하기"
date:                  2024-01-20T17:36:45.322593-07:00
model:                 gpt-4-1106-preview
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
날짜를 문자열로 변환하는 것은, 날짜를 읽기 편한 텍스트 형태로 바꾸는 과정입니다. 프로그래머들은 로깅, 사용자 인터페이스, 데이터 저장 등을 위해 이 작업을 수행합니다.

## How to: (실행 방법:)
```haskell
import Data.Time

-- 날짜를 문자열로 변환하기
main :: IO ()
main = do
    currentTime <- getCurrentTime
    let dateString = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
    putStrLn dateString
```

예상 출력:
```
2023-03-15 14:55:01
```

## Deep Dive (심층 탐구)
날짜와 시간은 컴퓨터 과학에서 오래된 주제입니다. `Data.Time` 라이브러리는 Haskell에서 날짜와 시간을 다룰 때 기본적으로 사용하는 것으로, `formatTime` 함수는 기존의 C 언어에서의 `strftime` 함수에서 아이디어를 가져왔습니다.

`formatTime` 함수의 첫 번째 인자는 `TimeLocale`로, 이는 여러 지역과 언어에 맞게 날짜를 표현하는 방법을 정의합니다. 기본적으로 `defaultTimeLocale`을 사용하지만, 필요한 경우 사용자 정의도 가능합니다.

형식 문자열, 예를 들어 `"%Y-%m-%d %H:%M:%S"`는 각각 연도, 월, 일, 시간, 분, 초를 나타내는 방법을 지정합니다. 이는 매우 유연해 원하는 출력 포맷을 정의할 수 있습니다.

대안으로, `Data.Time.Format` 모듈을 사용하여 다른 포맷터를 사용할 수도 있습니다. 또한, 한국어와 같은 다른 언어에 대한 날짜 형식도 지원합니다.

## See Also (관련 자료)
- [Haskell Data.Time documentation](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html) - 공식 `Data.Time` 라이브러리 문서
- [Haskell Time Locale documentation](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html#v:defaultTimeLocale) - `TimeLocale`와 관련된 문서