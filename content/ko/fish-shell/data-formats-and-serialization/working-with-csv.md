---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:53.311092-07:00
description: "CSV(Comma Separated Values, \uC27C\uD45C\uB85C \uAD6C\uBD84\uB41C \uAC12\
  ) \uD30C\uC77C \uC791\uC5C5\uC740 \uD30C\uC2F1, \uC870\uC791 \uBC0F \uB370\uC774\
  \uD130 \uAD50\uD658\uC5D0 \uB110\uB9AC \uC0AC\uC6A9\uB418\uB294 \uD0ED \uD615\uC2DD\
  \uC758 \uB370\uC774\uD130 \uC0DD\uC131\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4. \uD504\
  \uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC774\uB7EC\uD55C \uC791\uC5C5\uC744 \uD1B5\
  \uD574 \uB370\uC774\uD130\uB97C \uD6A8\uC728\uC801\uC73C\uB85C \uCC98\uB9AC\uD558\
  \uACE0 \uBD84\uC11D\uD558\uBA70, \uC791\uC5C5\uC744 \uC790\uB3D9\uD654\uD558\uAC70\
  \uB098 \uB2E4\uB978 \uC2DC\uC2A4\uD15C\uACFC\u2026"
lastmod: '2024-03-13T22:44:55.888320-06:00'
model: gpt-4-0125-preview
summary: "CSV(Comma Separated Values, \uC27C\uD45C\uB85C \uAD6C\uBD84\uB41C \uAC12\
  ) \uD30C\uC77C \uC791\uC5C5\uC740 \uD30C\uC2F1, \uC870\uC791 \uBC0F \uB370\uC774\
  \uD130 \uAD50\uD658\uC5D0 \uB110\uB9AC \uC0AC\uC6A9\uB418\uB294 \uD0ED \uD615\uC2DD\
  \uC758 \uB370\uC774\uD130 \uC0DD\uC131\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4."
title: "CSV\uC640 \uD568\uAED8 \uC791\uC5C5\uD558\uAE30"
weight: 37
---

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
