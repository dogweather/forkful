---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:52.152125-07:00
description: "\uBC29\uBC95: Haskell\uC5D0\uC11C\uB294 `cassava` \uB77C\uC774\uBE0C\
  \uB7EC\uB9AC\uB97C \uC0AC\uC6A9\uD558\uC5EC CSV \uD30C\uC77C\uC744 \uCC98\uB9AC\uD560\
  \ \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uC774 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB294 \uC774\
  \ \uBAA9\uC801\uC744 \uC704\uD55C \uC778\uAE30 \uC788\uB294 \uD0C0\uC0AC \uB77C\uC774\
  \uBE0C\uB7EC\uB9AC \uC911 \uD558\uB098\uC785\uB2C8\uB2E4. \uC544\uB798 \uC608\uC2DC\
  \uB4E4\uC740 `cassava`\uB97C \uC0AC\uC6A9\uD558\uC5EC CSV \uD30C\uC77C\uC744 \uC77D\
  \uACE0 \uC4F0\uB294 \uBC29\uBC95\uC744 \uBCF4\uC5EC\uC90D\uB2C8\uB2E4. **1.\u2026"
lastmod: '2024-03-13T22:44:55.328175-06:00'
model: gpt-4-0125-preview
summary: "Haskell\uC5D0\uC11C\uB294 `cassava` \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C\
  \ \uC0AC\uC6A9\uD558\uC5EC CSV \uD30C\uC77C\uC744 \uCC98\uB9AC\uD560 \uC218 \uC788\
  \uC2B5\uB2C8\uB2E4."
title: "CSV\uC640 \uD568\uAED8 \uC791\uC5C5\uD558\uAE30"
weight: 37
---

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
