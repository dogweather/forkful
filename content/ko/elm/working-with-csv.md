---
title:                "CSV 파일 작업"
html_title:           "Elm: CSV 파일 작업"
simple_title:         "CSV 파일 작업"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/working-with-csv.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

CSV를 처리하는 것은 데이터를 정리하고 분석하는 일상적인 프로그래밍 작업입니다. 프로그래머들은 CSV를 사용하여 텍스트 파일 형식으로 정형화된 데이터를 다룹니다. 데이터베이스를 사용하지 않는 간단한 데이터 저장 방식으로써, CSV는 효율적이고 다양한 프로그램에서 사용할 수 있어 매우 유용합니다.

## 방법:

```Elm
import Csv

csvData = """
이름, 나이, 성별
제이슨, 34, 남성
테일러, 28, 여성
"""

parsed = Csv.parse csvData
-- 결과: Result.Ok [["이름", "나이", "성별"], ["제이슨", "34", "남성"], ["테일러", "28", "여성"]]

```

## 깊이 파헤치기:

CSV는 1972년 미국의 데이터베이스 전문가 존 존슨에 의해 개발되었습니다. 그 후로도 널리 사용되는 데이터 저장 방식으로 알려졌지만, 현재는 더 효율적인 데이터베이스 시스템으로 대체되어 많은 프로그래머들이 더 효율적인 방식을 찾고 있습니다. 하지만 간단한 데이터를 정리하거나 텍스트 파일로 저장할 때에는 여전히 유용한 방법입니다. Elm에서는 표준 라이브러리인 Csv 모듈을 통해 쉽게 CSV를 처리할 수 있습니다.

## 관련 정보:

- [Elm 공식 문서 - Csv 모듈](https://package.elm-lang.org/packages/elm/core/latest/Csv)
- [CSV 정보 저널 - 개발자들을위한 CSV 가이드](https://www.csvj.com/csv-tutorial-for-developers/)