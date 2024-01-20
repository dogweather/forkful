---
title:                "CSV 파일 다루기"
html_title:           "Arduino: CSV 파일 다루기"
simple_title:         "CSV 파일 다루기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV는 'Comma-Separated Values'의 약자로 데이터를 컴마로 구분한 텍스트 파일입니다. 프로그래머는 데이터 교환과 저장을 위해 CSV를 자주 사용합니다.

## How to:
CSV 파일에서 데이터를 읽고, 처리하고, 쓰는 기본적인 Fish Shell 명령어들입니다.

```Fish Shell
# CSV 파일 읽기
cat data.csv

# 특정 열(예: 2번째)을 추출
cut -d ',' -f 2 data.csv

# 데이터 정렬
sort data.csv

# 특정 단어를 포함하는 행만 추출 (예: "Seoul")
grep Seoul data.csv

# CSV 파일 생성
echo 'name,age,city' > new_data.csv
echo 'Alice,30,Seoul' >> new_data.csv
cat new_data.csv
```

출력 예시:

```
name,age,city
Alice,30,Seoul
```

## Deep Dive
CSV는 1972년 IBM에서 처음 나왔습니다. 대안으로는 JSON, XML 등이 있으며, 데이터의 복잡성과 구조에 따라 선택할 수 있습니다. Fish Shell에서는 `cut`, `sort`, `grep` 등의 유닉스 명령어를 통해 CSV 처리가 간단하게 수행됩니다.

## See Also
- Fish Shell 공식 문서: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- Unix 명령어 튜토리얼: [https://www.gnu.org/software/coreutils/manual](https://www.gnu.org/software/coreutils/manual)
- CSV 관련 RFC 문서: [https://tools.ietf.org/html/rfc4180](https://tools.ietf.org/html/rfc4180)