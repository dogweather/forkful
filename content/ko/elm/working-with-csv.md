---
title:                "CSV 파일 작업하기"
html_title:           "Elm: CSV 파일 작업하기"
simple_title:         "CSV 파일 작업하기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/working-with-csv.md"
---

{{< edit_this_page >}}

## 왜

CSV는 쉼표로 구분된 값 (Comma-Separated Values)의 약어로, 데이터를 저장하고 전송하는 데에 자주 사용됩니다. Elm에서 사용되는 CSV는 데이터 처리 및 분석을 위해 유용한 형식이며, 그 검색 능력과 대량 데이터 처리 능력은 모두 반갑습니다. 이 글에서는 Elm을 사용하여 CSV 파일을 다루는 방법을 살펴보겠습니다.

## 사용 방법

Elm에서 CSV 데이터를 다루는 것은 매우 간단합니다. CSV 라이브러리인 `elm-explorations/csv`를 사용하여 데이터를 로드하고 파싱 할 수 있습니다.

```Elm
import Csv
import Maybe

-- 파싱할 CSV 파일
csvFile : String
csvFile =
  """
  Name, Age, Occupation, Salary
  John, 25, Developer, 1000
  Alice, 30, Designer, 800
  """

-- 데이터를 로드하고 파싱
parsedData : Maybe (List (List String))
parsedData =
  Csv.fromString csvFile

-- 데이터 출력
case parsedData of
  Just data ->
    data
      |> List.map (\row ->
        String.join " | " row
      )
      |> String.join "\n"
      |> Debug.log "Output"

  Nothing ->
    Debug.log "Failed to parse CSV file"
```

위의 코드에서 `Csv.fromString` 함수는 `Maybe (List (List String))` 타입을 반환하는데, 이는 CSV 데이터를 정확하게 파싱할 수 없을 경우 `Nothing`을 반환할 수도 있기 때문입니다. 따라서 `Maybe` 타입을 `case`로 처리하여 데이터를 출력하거나 처리할 수 있습니다.

출력은 다음과 같은 형태가 될 것입니다.

```
Output:
Name | Age | Occupation | Salary
John | 25 | Developer | 1000
Alice | 30 | Designer | 800
```

## 깊게 파보기

CSV 파일을 다루기 위해서는 `elm-explorations/csv` 라이브러리의 `Csv.Decode` 모듈을 사용하여 데이터를 파싱해야 합니다. 이 모듈은 기본 타입인 `String`, `Int`, `Float`, `Bool` 외에도 `list`, `record`, `maybe` 등 다양한 데코더를 제공합니다. 이를 사용하여 복잡한 구조의 CSV 데이터도 쉽게 처리할 수 있습니다.

또한, CSV 데이터를 다루는 데에는 `List`, `String` 등의 Elm의 기본 타입 외에도 `Json.Encode` 모듈을 사용하여 데이터를 다룰 수도 있습니다. 따라서 필요한 경우에는 이 모듈을 사용하여 데이터 처리를 할 수도 있습니다.

## 더 알아보기

- [Elm 공식 홈페이지](https://elm-lang.org/)
- [elm-explorations/csv 라이브러리 문서](https://package.elm-lang.org/packages/elm-explorations/csv/latest/)
- [Json.Decode 모듈 문서](https://package.elm-lang.org/packages/elm/json/latest/Json-Decode)