---
title:                "CSV 파일 작업하기"
html_title:           "Haskell: CSV 파일 작업하기"
simple_title:         "CSV 파일 작업하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

CSV 작업이란 무엇인가요? 이는 일반 텍스트를 통해 표 형식 데이터를 저장하고 전송하기 위한 방법입니다. 프로그래머들은 CSV 파일을 이용하여 데이터를 손쉽게 읽고 쓸 수 있기 때문에 이를 사용합니다.

## 방법:

Haskell에서 CSV 작업을 하는 방법은 간단합니다. 우선, `Data.Csv` 모듈을 `import` 한 다음, `decode` 함수를 사용하여 CSV 파일을 읽을 수 있습니다. 아래는 간단한 예시 코드입니다.
```Haskell
import Data.Csv

main = do
    csvData <- readFile "data.csv" -- CSV 파일을 읽어옴
    case decode HasHeader csvData of -- CSV 파일을 해석하여 리스트 형태로 저장
        Left err -> putStrLn err -- 오류 발생시 오류 메시지 출력
        Right data -> print data -- 성공 시 데이터 출력
```
위 코드는 `data.csv` 파일을 읽어와서 파일에 헤더가 있는 경우 이를 리스트 형태로 저장하고, 오류가 발생할 경우 그 오류 메시지를 출력하며, 성공할 경우 데이터를 출력합니다.

## 깊이 알아보기:

CSV 파일 포맷은 1972년에 처음 소개된 이후 온라인으로의 데이터 전송에 매우 유용하게 사용되었습니다. 현재는 보다 더 발전된 형태의 데이터 포맷이 있지만, 여전히 많은 프로그램들이 CSV 파일을 사용합니다. Haskell에서는 `cassava` 라이브러리를 통해 CSV 작업을 할 수 있습니다. 이 라이브러리는 내부적으로 파서 라이브러리인 `attoparsec`를 사용하므로, CSV 파일을 해석하는 과정에서 더 재미있는 일들을 할 수도 있습니다.

## 관련 링크:

- Haskell CSV 라이브러리: https://hackage.haskell.org/package/cassava
- 파서 라이브러리인 attoparsec: https://hackage.haskell.org/package/attoparsec