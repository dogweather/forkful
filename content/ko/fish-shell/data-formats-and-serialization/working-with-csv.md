---
title:                "CSV와 함께 작업하기"
aliases:
- /ko/fish-shell/working-with-csv.md
date:                  2024-02-03T19:19:53.311092-07:00
model:                 gpt-4-0125-preview
simple_title:         "CSV와 함께 작업하기"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

CSV(Comma Separated Values, 쉼표로 구분된 값) 파일 작업은 파싱, 조작 및 데이터 교환에 널리 사용되는 탭 형식의 데이터 생성을 포함합니다. 프로그래머들은 이러한 작업을 통해 데이터를 효율적으로 처리하고 분석하며, 작업을 자동화하거나 다른 시스템과 통합합니다.

## 어떻게:

Fish Shell은 CSV 조작을 위해 특별히 설계된 내장 함수를 직접 제공하지 않습니다. 하지만, `awk`, `sed`, `cut`과 같은 유닉스 유틸리티를 기본 작업에 활용하거나, 좀 더 고급 작업을 위해 `csvkit`과 같은 전문 도구를 사용할 수 있습니다.

### CSV 파일 읽기 및 첫 번째 열 출력:
`cut`을 사용하여 첫 번째 열 추출:
```fish
cut -d ',' -f1 data.csv
```
출력 예시:
```
Name
Alice
Bob
```

### 열 값에 기반한 CSV 행 필터링:
두 번째 열이 "42"와 일치하는 행을 찾기 위해 `awk` 사용:
```fish
awk -F, '$2 == "42" { print $0 }' data.csv
```
출력 예시:
```
Bob,42,London
```

### CSV 파일 수정하기 (예: 열 추가하기):
"NewColumn"이라는 정적 값으로 열 추가하기 위해 `awk` 사용:
```fish
awk -F, 'BEGIN {OFS=","} {print $0,"NewColumn"}' data.csv > modified.csv
```
`modified.csv`에서 출력 예시:
```
Name,Age,City,NewColumn
Alice,30,New York,NewColumn
Bob,42,London,NewColumn
```

### 좀 더 고급 작업을 위한 `csvkit` 사용하기:
먼저, `csvkit`이 설치되었는지 확인하세요. 설치되어있지 않다면 pip을 사용하여 설치하세요: `pip install csvkit`.

**CSV 파일을 JSON으로 변환하기:**
```fish
csvjson data.csv > data.json
```
`data.json`에서 출력 예시:
```json
[{"Name":"Alice","Age":"30","City":"New York"},{"Name":"Bob","Age":"42","City":"London"}]
```

**`csvkit`의 `csvgrep`을 사용한 필터링:**
```fish
csvgrep -c 2 -m 42 data.csv
```
이 명령어는 `csvkit`를 사용하여 필터링 작업을 복제하고, 값 "42"를 위해 열 2를 대상으로 합니다.

결론적으로, Fish Shell 자체가 직접적인 CSV 조작 기능을 제공하지는 않지만, 유닉스 유틸리티와 `csvkit`과 같은 도구의 이용 가능성을 통해 CSV 파일 작업을 위한 강력한 옵션을 제공합니다.
