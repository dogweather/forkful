---
title:                "CSV와 함께 작업하기"
aliases:
- /ko/haskell/working-with-csv/
date:                  2024-02-03T19:19:52.152125-07:00
model:                 gpt-4-0125-preview
simple_title:         "CSV와 함께 작업하기"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

CSV(Comma-Separated Values, 쉼표로 구분된 값) 작업은 간단한 텍스트 기반 형식으로 표 데이터를 저장하는 파일을 파싱하고 생성하는 것을 포함합니다. 프로그래머들은 스프레드시트, 데이터베이스에서 데이터를 효율적으로 가져오거나 내보내거나 다른 프로그램 간의 데이터 교환을 용이하게 하기 위해 이 작업에 자주 참여합니다.

## 방법:

Haskell에서는 `cassava` 라이브러리를 사용하여 CSV 파일을 처리할 수 있습니다. 이 라이브러리는 이 목적을 위한 인기 있는 타사 라이브러리 중 하나입니다. 아래 예시들은 `cassava`를 사용하여 CSV 파일을 읽고 쓰는 방법을 보여줍니다.

**1. CSV 파일 읽기:**

먼저, 프로젝트의 cabal 파일에 추가하거나 Stack을 사용하여 `cassava`가 설치되었는지 확인하세요.

다음은 CSV 파일을 읽고 각 레코드를 출력하는 간단한 예제입니다. CSV 파일에는 이름과 나이라는 두 개의 열이 있다고 가정합니다.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

main :: IO ()
main = do
    csvData <- BL.readFile "people.csv"
    case decode NoHeader csvData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \(name, age) ->
            putStrLn $ name ++ " 은(는) " ++ show (age :: Int) ++ "살입니다."
```

`people.csv`가 다음과 같다고 가정합니다:
```
John,30
Jane,25
```
출력될 내용은 다음과 같습니다:
```
John 은(는) 30살입니다.
Jane 은(는) 25살입니다.
```

**2. CSV 파일 쓰기:**

CSV 파일을 생성하려면 `cassava`의 `encode` 함수를 사용할 수 있습니다.

다음은 레코드 목록을 CSV 파일에 쓰는 방법입니다:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Csv
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = BL.writeFile "output.csv" $ encode [("John", 30), ("Jane", 25)]
```

이 프로그램을 실행한 후에 `output.csv`는 다음과 같은 내용을 담게 됩니다:

```
John,30
Jane,25
```

이 간결한 소개를 통해 Haskell에서 `cassava` 라이브러리를 사용하여 CSV 파일을 읽고 쓰는 방법을 보여줌으로써, 언어에 새로운 이들도 데이터 조작 작업을 더 쉽게 접근할 수 있게 됩니다.
