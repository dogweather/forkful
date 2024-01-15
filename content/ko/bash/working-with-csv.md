---
title:                "CSV 작업하기"
html_title:           "Bash: CSV 작업하기"
simple_title:         "CSV 작업하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/working-with-csv.md"
---

{{< edit_this_page >}}

## 왜

CSV 파일을 다루는 작업을 하는 이유는 데이터를 관리하고 분석하기 위해서입니다. CSV 파일은 일반적으로 엑셀과 같은 프로그램에서 열어 볼 수 있기 때문에 많은 사람들이 사용하고 있습니다.

## 어떻게

CSV 파일을 다루기 위해서는 Bash의 다양한 기능을 이용할 수 있습니다. 먼저, 파일을 읽어오고 필요한 정보를 추출하는 방법을 살펴보겠습니다. 

```Bash
# 파일 읽어오기
file="sample.csv"
while read line; do
  echo "$line"
done < "$file"

# 필요한 정보 추출하기
file="sample.csv"
while IFS="," read col1 col2 col3; do
  echo "$col1"
  echo "$col2"
  echo "$col3"
done < "$file"

```

위의 예시 코드에서는 CSV 파일을 한 줄씩 읽어오며, 필요한 정보를 변수에 저장합니다. 따라서 필요에 따라 읽어온 정보를 활용할 수 있습니다. 이외에도 Bash에서는 다양한 기능을 이용해 CSV 파일을 다룰 수 있습니다. 

## 딥 다이브

Bash에서는 CSV 파일을 다루기 위해 다음과 같은 기능을 제공합니다.

- 파일 읽기 및 쓰기: `read`, `echo`, `printf` 등의 기능을 이용해 파일을 읽고 쓸 수 있습니다.
- 데이터 추출: `cut`, `awk` 등의 기능을 이용해 필요한 정보만 추출할 수 있습니다.
- 대체값 지정: `sed`, `tr` 등의 기능을 이용해 파일 내의 요소를 대체하거나 삭제할 수 있습니다.
- 조건문 처리: `if`, `case` 등의 기능을 이용해 조건에 따른 처리가 가능합니다.

이외에도 Bash에서는 더 다양한 기능을 제공하고 있으며, 적절한 사용법을 숙지하면 유연하게 CSV 파일을 다룰 수 있습니다.

## 참고자료

- [Bash 공식 문서](https://www.gnu.org/software/bash/manual/bash.html)
- [Bash Script를 이용한 CSV 파일 다루기](https://www.computerhope.com/unix/bash/read.htm)
- [Bash에서 CSV 파일 다루기](https://unix.stackexchange.com/questions/159950/how-to-use-bash-to-extract-data-from-a-csv-file/159954#159954)

-----


## 참고

- [Bash 공식 문서](https://www.gnu.org/software/bash/manual/bash.html)
- [Bash Script를 이용한 CSV 파일 다루기](https://www.computerhope.com/unix/bash/read.htm)
- [Bash에서 CSV 파일 다루기](https://unix.stackexchange.com/questions/159950/how-to-use-bash-to-extract-data-from-a-csv-file/159954#159954)