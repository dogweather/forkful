---
title:                "CSV 파일 다루기"
date:                  2024-01-19
html_title:           "Arduino: CSV 파일 다루기"
simple_title:         "CSV 파일 다루기"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
CSV(Comma-Separated Values)는 간단한 파일 형식으로, 테이블 데이터를 저장합니다. 프로그래머들은 CSV를 다루어 대량의 데이터를 쉽게 이동, 복사, 분석하기 위해 사용합니다.

## How to: (어떻게 할까?)
```Haskell
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Csv

type Person = (String, Int, String)

decodePeople :: BL.ByteString -> Either String (V.Vector Person)
decodePeople = fmap snd . decode NoHeader

main :: IO ()
main = do
  csvData <- BL.readFile "people.csv"
  case decodePeople csvData of
    Left err -> putStrLn err
    Right v -> V.forM_ v $ \(name, age, address) ->
      putStrLn $ name ++ " is " ++ show age ++ " years old and lives at " ++ address
```

Sample `people.csv`:
```
John Doe,30,123 Elm St
Jane Smith,25,456 Oak St
```

Sample output:
```
John Doe is 30 years old and lives at 123 Elm St
Jane Smith is 25 years old and lives at 456 Oak St
```

## Deep Dive (깊이 파보기)
- CSV 형식은 1972년 IBM에서 처음 사용되었습니다.
- 표준 라이브러리를 사용하는 것 대신 `cassava`와 같은 패키지를 활용할 수 있습니다.
- CSV 파싱은 `ByteString`과 `Vector`를 사용함으로써 메모리 효율과 속도를 개선할 수 있습니다.

## See Also (더 보기)
- Haskell `cassava` library: [https://hackage.haskell.org/package/cassava](https://hackage.haskell.org/package/cassava)
- More on CSV format: [https://tools.ietf.org/html/rfc4180](https://tools.ietf.org/html/rfc4180)
- Working with ByteString: [https://hackage.haskell.org/package/bytestring](https://hackage.haskell.org/package/bytestring)
