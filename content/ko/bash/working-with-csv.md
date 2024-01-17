---
title:                "CSV 파일 다루기"
html_title:           "Bash: CSV 파일 다루기"
simple_title:         "CSV 파일 다루기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/working-with-csv.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

CSV를 다룬다는 것은 데이터를 쉽게 관리하고 공유하기 위한 일반적인 방법입니다. 많은 프로그래머들이 CSV 형식의 데이터를 사용하는 이유는 그것이 다른 컴퓨터 프로그램에서 쉽게 읽을 수 있기 때문입니다. 이것은 데이터 내에서 공백 및 특수 문자를 처리하지 않고도 데이터 값을 유지할 수 있기 때문입니다.

## 진행 방법:

```Bash
# CSV 파일 만들기
touch data.csv
# CSV 파일 열기
vim data.csv
# 데이터 입력하기
1,2,3,4,5
a,b,c,d,e
6,7,8,9,10

# CSV 파일 읽기
while IFS=',' read -r col1 col2 col3 col4 col5; do
  echo "col1: $col1, col2: $col2, col3: $col3, col4: $col4, col5: $col5"
done < data.csv

# 출력 예시
col1: 1, col2: 2, col3: 3, col4: 4, col5: 5
col1: a, col2: b, col3: c, col4: d, col5: e
col1: 6, col2: 7, col3: 8, col4: 9, col5: 10
```

## 깊이 들어가기:

CSV 형식은 1950년대에 IBM의 조직 및 개발팀에서 개발되었습니다. 이 형식은 여러 개의 속성을 가진 데이터를 효율적으로 저장하고 액세스하는 것을 목적으로 만들어졌습니다. CSV 파일을 다루는 대안으로는 JSON과 XML이 있지만, 간단한 데이터를 다룰 때는 CSV가 더 적합합니다. Bash에서는 CSV 파일을 읽고 쓰는 데 필요한 명령어들이 내장되어 있기 때문에, 따로 추가적인 패키지를 설치할 필요가 없습니다.

## 더 알아보기:

- [CSV 형식 정의](https://tools.ietf.org/html/rfc4180)
- [GNU Bash 매뉴얼](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html)