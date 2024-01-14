---
title:                "Haskell: csv 작업하기"
simple_title:         "csv 작업하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## 왜

CSV 파일을 다루는데에는 어떤 이유가 있을까요? CSV 파일은 엑셀 등 스프레드시트 프로그램에서 자주 사용되는 파일 형식입니다. 이러한 파일 형식을 다룰 수 있는 프로그래밍 언어는 데이터 가공이나 분석 등에 유용하게 사용될 수 있습니다.

## 어떻게

CSV 파일을 다루는 방법에 대해 살펴보겠습니다. 먼저, Haskell에서 CSV 파일을 읽어오는 방법은 다음과 같습니다.

```
import Text.CSV
import Control.Monad.Trans.Except

main :: IO ()
main = do
    -- CSV 파일을 읽어오기
    file <- readFile "data.csv"
    -- CSV 파일을 파싱하여 리스트 형태로 얻기
    let records = parseCSV "data.csv" file :: Either (String, CSV.ParseError) [[String]]

    -- records 변수에 대한 패턴 매칭을 통해 데이터 가공
    case records of
        Left err -> putStrLn $ "파싱 에러 발생: " ++ show err
        Right csv -> do
            putStrLn "읽어온 CSV 파일 내용:"
            mapM_ print csv

-- 출력 예시:
-- "읽어온 CSV 파일 내용:"
-- ["이름","나이","성별"]
-- ["John","25","남성"]
-- ["Jane","30","여성"]
```

## 딥 다이브

CSV 파일을 다루는데에는 더 많은 기능이 있습니다. 이 예제에서는 파일을 읽어오는 방법만 다루었지만, CSV 파일을 쓰는 방법도 유사하게 구현할 수 있습니다. 또한, 데이터를 필터링하거나 정렬하는 등의 기능도 포함될 수 있습니다. 이러한 기능들은 다양한 라이브러리를 사용하거나 직접 구현할 수 있습니다.

## 더 많은 정보

- [Haskell CSV 라이브러리](https://hackage.haskell.org/package/csv)

--

## 더 찾아보기

- [Haskell 레퍼런스](https://wiki.haskell.org/Haskell)
- [Haskell 튜토리얼](http://learnyouahaskell.com/chapters)
- [Haskell 코딩 예제](http://www.codewars.com/kata/search/haskell?beta=false)
- [Haskell 커뮤니티](https://www.reddit.com/r/haskell/)