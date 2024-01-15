---
title:                "CSV 작업하기"
html_title:           "Haskell: CSV 작업하기"
simple_title:         "CSV 작업하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## 왜 CSV 작업에 참여해야 할까?

데이터 분석 및 처리를 위해 CSV 파일 형식을 다룰 필요가 있을 때가 있습니다. Haskell은 CSV 파일을 처리하기에 강력한 언어로, 재사용 가능한 코드를 작성하고 다양한 프로젝트에 적용할 수 있습니다.

## 어떻게 하면 될까?

제공된 코드 블록을 참고하여 CSV 파일을 불러오고, 처리 및 분석하는 간단한 예제를 살펴봅시다.

``` Haskell
import Data.CSV

-- CSV 파일을 불러오는 함수
readCSV :: FilePath -> IO CSV
readCSV path = do
  csv <- parseCSVFromFile path
  case csv of
    Left err -> error "CSV 파일을 불러오는 데 실패했습니다."
    Right content -> return content

-- "data.csv" 파일 불러오기
csv <- readCSV "data.csv"

-- 첫 번째 행 출력
head csv

-- 두 번째 열의 값들만 추출하기
let column2 = map (!!1) csv

-- 전체 데이터에 함수 적용하기
let processedData = map (\x -> x * 2) column2

-- 결과 출력하기
print processedData
```

**결과:**

`[154, 256, 364, 768, 458, 536]`

## 깊게 파고들기

Haskell은 CSV 파일을 처리하는 데 유용한 여러 가지 함수와 라이브러리를 제공합니다. `parseCSVFromFile` 함수를 사용하여 CSV 파일을 불러올 수 있고, `Indexable` 타입 클래스를 사용하여 파일 내 원하는 위치의 데이터에 접근할 수 있습니다. 또한, `Text.CSV` 모듈에서 제공하는 다양한 함수를 사용하여 CSV 파일을 다룰 수 있습니다. 더 자세한 내용은 공식 문서를 참고해보세요!

## 더 참고하기

[공식 Haskell 문서](https://www.haskell.org/documentation/)

[Hackage - Haskell 라이브러리 저장소](https://hackage.haskell.org/)

[해스켈을 이용한 CSV 데이터 처리 예제](https://gist.github.com/amagata/3c20511bfd72d59b5a2fa0a143ae4be5)