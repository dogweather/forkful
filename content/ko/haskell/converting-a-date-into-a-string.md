---
title:    "Haskell: 날짜를 문자열로 변환하기"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## 왜

누군가 날짜를 문자열로 변환하는 일에 참여하는 이유는 무엇일까요?

## 어떻게 하나요?

날짜를 문자열로 변환하는 것은 가장 일반적인 작업 중 하나입니다. Haskell을 사용하여 간단한 코드로 쉽게 할 수 있습니다. 다음 예제와 출력을 확인해보세요.

```Haskell
import Data.Time.Format

-- 오늘 날짜를 문자열로 변환하는 함수
dateToString :: IO()
dateToString = do
  current <- getCurrentTime
  let formatted = formatTime defaultTimeLocale "%B %d, %Y" current
  putStrLn (show formatted)

--- 출력 예제: "April 16, 2020"
```

이 예제에서는 `Data.Time.Format` 모듈에서 날짜 형식을 포맷하는 데 사용하는 함수를 가져와 `dateToString`이라는 함수로 정의합니다. `getCurrentTime` 함수로 현재 시간을 가져온 후 `formatTime` 함수를 사용하여 원하는 형식으로 날짜를 변환합니다. 마지막으로 문자열로 변환된 날짜를 콘솔에 출력합니다.

이 코드를 실행하면 현재 날짜를 문자열로 변환하는 것을 확인할 수 있습니다.

## 깊게 파헤쳐보기

날짜를 다른 문자열 포맷으로 변환하는 방법 외에도 `Data.Time.Format` 모듈에는 `parseTimeM` 함수를 사용하여 문자열을 날짜로 변환하는 기능도 있습니다. 다음 예제를 살펴보세요.

```Haskell
-- 문자열을 날짜로 변환하는 함수
stringToDate :: String -> IO()
stringToDate str = do
  let formatted = parseTimeM True defaultTimeLocale "%B %d, %Y" str :: Maybe UTCTime
  case formatted of
    Just x -> putStrLn (show x)
    Nothing -> putStrLn "Invalid date format! Please try again."

--- 입력 예제: "April 16, 2020"
--- 출력 예제: "2020-04-16 00:00:00 UTC"
```

이번에는 `parseTimeM` 함수를 사용하고 있습니다. 이 함수는 문자열과 매치될 수 있는 포맷을 입력으로 받아 문자열을 날짜로 변환합니다. `:: Maybe UTCTime`을 사용하여 반환 값이 `Maybe` 타입으로 선언되어 있으므로 "Maybe" 형식으로 값을 판단해야 합니다. `case` 문을 사용하여 변환된 값이 `Just`인지 `Nothing`인지에 따라 다른 출력 결과를 나타내도록 합니다.

## 더 알아보기

한국어로 작성된 Haskell 공식 문서에 날짜와 관련된 자세한 정보가 있습니다. 프로젝트에 맞게 다양한 형식으로 날짜를 변환할 수 있도록 다양한 기능을 제공하고 있으니 참고하시기 바랍니다.

[Haskell 공식 문서 - Data.Time.Format](https://www.haskell.org/hoogle/?hoogle=Data.Time.Format)

---

## 관련 링크

* [Haskell 공식 문서 - Data.Time.Format](https://www.haskell.org/hoogle/?hoogle=Data.Time.Format)