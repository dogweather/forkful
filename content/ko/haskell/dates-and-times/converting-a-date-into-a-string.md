---
date: 2024-01-20 17:36:45.322593-07:00
description: "\uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\uD558\uB294\
  \ \uAC83\uC740, \uB0A0\uC9DC\uB97C \uC77D\uAE30 \uD3B8\uD55C \uD14D\uC2A4\uD2B8\
  \ \uD615\uD0DC\uB85C \uBC14\uAFB8\uB294 \uACFC\uC815\uC785\uB2C8\uB2E4. \uD504\uB85C\
  \uADF8\uB798\uBA38\uB4E4\uC740 \uB85C\uAE45, \uC0AC\uC6A9\uC790 \uC778\uD130\uD398\
  \uC774\uC2A4, \uB370\uC774\uD130 \uC800\uC7A5 \uB4F1\uC744 \uC704\uD574 \uC774 \uC791\
  \uC5C5\uC744 \uC218\uD589\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.312296-06:00'
model: gpt-4-1106-preview
summary: "\uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\uD558\uB294 \uAC83\
  \uC740, \uB0A0\uC9DC\uB97C \uC77D\uAE30 \uD3B8\uD55C \uD14D\uC2A4\uD2B8 \uD615\uD0DC\
  \uB85C \uBC14\uAFB8\uB294 \uACFC\uC815\uC785\uB2C8\uB2E4."
title: "\uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\uD558\uAE30"
weight: 28
---

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
