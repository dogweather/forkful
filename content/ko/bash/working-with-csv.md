---
title:                "CSV 파일 다루기"
date:                  2024-01-19
html_title:           "Arduino: CSV 파일 다루기"
simple_title:         "CSV 파일 다루기"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
CSV(Comma-Separated Values)는 데이터를 저장하는 텍스트 형식이다. 프로그래머는 이를 이용해 데이터를 쉽게 교환하고, 다양한 프로그램에서 호환할 수 있게 사용한다.

## How to: (어떻게 하나?)
```Bash
# CSV 파일 읽기
while IFS=, read -r col1 col2 col3
do
  echo "Column 1: $col1 - Column 2: $col2 - Column 3: $col3"
done < input.csv

# CSV 파일에 쓰기
echo "data1,data2,data3" >> output.csv

# 출력 예시
Column 1: data1 - Column 2: data2 - Column 3: data3
```

## Deep Dive (심층 탐구)
CSV는 1972년 IBM의 포트란용 라이브러리에서 처음 등장했다. JSON, XML 같은 대안 형식들이 존재하지만, CSV는 가독성과 간결함으로 여전히 인기가 있다. Bash에서는 `cut`, `awk`, `sed` 같은 텍스트 처리 도구로 CSV를 다룰 수 있으나, 복잡한 CSV 처리에는 이러한 도구들의 한계가 있다.

## See Also (참조)
- [CSV 관련 RFC 문서](https://tools.ietf.org/html/rfc4180)
- [GNU awk 매뉴얼](https://www.gnu.org/software/gawk/manual/gawk.html)
- [Bash scripting cheatsheet](https://devhints.io/bash)
