---
title:                "문자열에서 날짜 분석하기"
html_title:           "Gleam: 문자열에서 날짜 분석하기"
simple_title:         "문자열에서 날짜 분석하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 필요한가?
문자열에서 날짜를 파싱하는 것은 문자열 형태의 날짜를 프로그램이 이해할 수 있는 날짜 데이터 형식으로 변환하는 것입니다. 프로그래머들이 이를 사용하는 이유는 다양한 원본에서 받은 날짜 정보를 프로그램 내에서 일관되게 처리하기 위해서입니다.

## 다음과 같이 해보세요:
날짜 파싱은 `Data.Time` 라이브러리를 활용할 수 있습니다. 다음은 "DD-MM-YYYY" 형식의 문자열을 `Day` 오브젝트로 파싱하는 간단한 예입니다:

```Haskell
import Data.Time

parseDate :: String -> Maybe Day
parseDate input = parseTimeM True defaultTimeLocale "%d-%m-%Y" input
```
예시를 실행해봅시다:

```Haskell
main :: IO ()
main = print $ parseDate "21-01-2021"
```

이 경우 출력의 결과는 `Just 2021-01-21`이 됩니다.

## 깊이 알아보기
사실, 문자열에서 날짜를 파싱하는 방법은 많이 있습니다.

1. **역사적 맥락**: 프로그래밍의 초기 단계에서는 이러한 날짜 변환을 수동으로 수행했지만, 복잡성과 에러 가능성이 높아진 라이브러리가 개발되기 시작했습니다.

2. **대안**: Haskell에서는 `Data.Time` 외에도 `time` 라이브러리나 `date` 라이브러리 등 다양한 라이브러리가 있습니다. 그러나 `Data.Time` 라이브러리는 표준 라이브러리로 가장 널리 사용됩니다.

3. **구현 세부사항**: 위의 코드에서 `parseTimeM` 함수는 문자열을 파싱하고 `Just Day` 또는 `Nothing`을 반환합니다. 이는 실패할 수 있는 파싱 작업을 수행하기 때문입니다. 만일 주어진 문자열이 유효한 날짜가 아니라면 `Nothing`을 반환합니다.

## 참고자료
- `Data.Time` 라이브러리의 Haskell hackage: [https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- Haskell에서 날짜와 시간 다루기: [https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/time#parsing-and-formatting-dates-and-times](https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/time#parsing-and-formatting-dates-and-times)