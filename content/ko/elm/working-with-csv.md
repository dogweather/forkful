---
title:                "CSV 파일 다루기"
date:                  2024-01-19
html_title:           "Arduino: CSV 파일 다루기"
simple_title:         "CSV 파일 다루기"

category:             "Elm"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV(Comma-Separated Values)는 데이터를 저장하고 전송하는 포맷입니다. 프로그래머들은 테이블 형태의 데이터를 쉽게 교환하고 분석하기 위해 사용합니다.

## How to:
CSV 파일을 읽고 쓰는 기본적인 예제입니다.

```Elm
import Csv

type alias User =
    { name : String
    , age : Int
    }

userDecoder : Decode.Decoder User
userDecoder =
    Decode.map2 User
        (Decode.field "name" Decode.string)
        (Decode.field "age" Decode.int)

decodeCsvString : String -> Result String (List User)
decodeCsvString csvString =
    csvString
        |> Csv.decode { delimiter = ',', quoteChar = '\"' }
        |> Result.mapError String.fromList
        |> Result.andThen (Csv.Decode.decode Csv.Decode.list userDecoder)

sampleCsv : String
sampleCsv =
    "name,age\nAlice,30\nBob,25"

-- 사용 예:
case decodeCsvString sampleCsv of
    Ok users ->
        -- 데이터 사용하기
        ...

    Err errorMessage ->
        -- 에러 처리하기
        ...
```

## Deep Dive
CSV는 1972년 IBM의 Fortran 버전에서 최초로 사용되었습니다. JSON이나 XML 같은 다른 데이터 포맷들이 있지만, CSV는 여전히 간단하고 대부분의 프로그래밍 언어와 소프트웨어에서 지원됩니다. Elm에서는 `Csv`와 `Csv.Decode` 모듈을 이용해 CSV 데이터를 작업할 수 있으며, 사용자 정의 타입으로의 변환을 위해 `Decoder`를 사용합니다.

## See Also
- Elm Decoder: [guide.elm-lang.org/interop/json.html](https://guide.elm-lang.org/interop/json.html)
