---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:52.152125-07:00
description: "CSV(Comma-Separated Values, \uC27C\uD45C\uB85C \uAD6C\uBD84\uB41C \uAC12\
  ) \uC791\uC5C5\uC740 \uAC04\uB2E8\uD55C \uD14D\uC2A4\uD2B8 \uAE30\uBC18 \uD615\uC2DD\
  \uC73C\uB85C \uD45C \uB370\uC774\uD130\uB97C \uC800\uC7A5\uD558\uB294 \uD30C\uC77C\
  \uC744 \uD30C\uC2F1\uD558\uACE0 \uC0DD\uC131\uD558\uB294 \uAC83\uC744 \uD3EC\uD568\
  \uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC2A4\uD504\uB808\
  \uB4DC\uC2DC\uD2B8, \uB370\uC774\uD130\uBCA0\uC774\uC2A4\uC5D0\uC11C \uB370\uC774\
  \uD130\uB97C \uD6A8\uC728\uC801\uC73C\uB85C \uAC00\uC838\uC624\uAC70\uB098 \uB0B4\
  \uBCF4\uB0B4\uAC70\uB098 \uB2E4\uB978 \uD504\uB85C\uADF8\uB7A8 \uAC04\uC758\u2026"
lastmod: '2024-03-13T22:44:55.328175-06:00'
model: gpt-4-0125-preview
summary: "CSV(Comma-Separated Values, \uC27C\uD45C\uB85C \uAD6C\uBD84\uB41C \uAC12\
  ) \uC791\uC5C5\uC740 \uAC04\uB2E8\uD55C \uD14D\uC2A4\uD2B8 \uAE30\uBC18 \uD615\uC2DD\
  \uC73C\uB85C \uD45C \uB370\uC774\uD130\uB97C \uC800\uC7A5\uD558\uB294 \uD30C\uC77C\
  \uC744 \uD30C\uC2F1\uD558\uACE0 \uC0DD\uC131\uD558\uB294 \uAC83\uC744 \uD3EC\uD568\
  \uD569\uB2C8\uB2E4."
title: "CSV\uC640 \uD568\uAED8 \uC791\uC5C5\uD558\uAE30"
weight: 37
---

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
