---
title:                "문자열에서 날짜 파싱하기"
date:                  2024-01-20T15:36:51.709676-07:00
html_title:           "Arduino: 문자열에서 날짜 파싱하기"
simple_title:         "문자열에서 날짜 파싱하기"

category:             "Haskell"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열에서 날짜를 파싱한다는 것은 텍스트 형태의 날짜를 프로그램이 이해할 수 있는 날짜 데이터로 변환하는 과정입니다. 이것은 사용자의 입력을 처리하거나 로그 데이터를 분석할 때 필요합니다.

## How to:
```Haskell
import Data.Time

-- 문자열에서 날짜로 파싱을 시도합니다.
parseDate :: String -> Maybe Day
parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%d"

-- 예시 사용
main :: IO ()
main = do
    let dateString = "2023-04-01"
    print $ parseDate dateString
```
출력:

```Haskell
Just 2023-04-01
```

## Deep Dive (심층 분석)
파싱은 1970년대 초반에 개발된 함수형 프로그래밍 언어에서 시작됐습니다. Haskell은 강력한 타입 시스템과 순수 함수형 언어임에도 불구하고, 날짜와 시간을 다루는 데 있어서 편리한 라이브러리를 제공합니다. `Data.Time` 모듈을 이용하면, 표준 시간 포맷을 다루고 조작하기 쉽습니다. 위 예시에서는 간단한 ISO 8601 포맷(`"%Y-%m-%d"`)을 사용했습니다. `parseTimeM` 함수는 실패할 수 있으므로 `Maybe` 타입을 사용하여 결과를 돌려줍니다. 

대안으로는 `time` 패키지 외에도 `Data.Time.Format`의 `parseTime` 또는 `readTime`을 사용할 수 있습니다. 또는 외부 라이브러리인 `Thyme`와 `Chronos`를 사용하는 방법도 있습니다.

구현 세부사항으로, `parseTimeM` 함수는 문자열을 분석할 때 올바른 날짜 포맷을 지정해야 합니다. 잘못된 포맷이 주어지면 `Nothing`을 반환합니다.

## See Also (관련 자료)
- `Data.Time` 모듈 문서: https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html
- Haskell `time` 패키지: https://hackage.haskell.org/package/time
- Haskell Wiki의 날짜와 시간 처리 방법: https://wiki.haskell.org/Working_with_time
- `Thyme` 라이브러리: https://hackage.haskell.org/package/thyme
- `Chronos` 라이브러리: https://hackage.haskell.org/package/chronos
