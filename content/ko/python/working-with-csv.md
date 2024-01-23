---
title:                "CSV 파일 다루기"
html_title:           "Arduino: CSV 파일 다루기"
simple_title:         "CSV 파일 다루기"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
CSV는 Comma-Separated Values의 약자로 데이터를 저장하기 위한 단순한 형식입니다. 프로그래머들은 CSV를 사용하여 데이터를 쉽게 읽고 쓸 수 있으며 다양한 프로그램 간에 호환성을 제공하기 때문에 사용합니다.

## How to: (어떻게 하나요?)
Python에서 CSV 파일을 다루는 기본적인 방법을 알아봅니다. 아래 코드는 CSV 파일을 읽고 쓰는 예시입니다.

```Python
# CSV 파일 쓰기
import csv

# header 포함 데이터 준비
header = ["이름", "나이", "도시"]
data = [
    ["김철수", 28, "서울"],
    ["이영희", 34, "부산"]
]

# 'people.csv' 파일을 쓰기 모드로 열기
with open('people.csv', 'w', newline='', encoding='utf-8') as csvfile:
    writer = csv.writer(csvfile)
    writer.writerow(header)
    writer.writerows(data)
    
# CSV 파일 읽기
import csv

# 'people.csv' 파일을 읽기 모드로 열기
with open('people.csv', 'r', encoding='utf-8') as csvfile:
    reader = csv.reader(csvfile)
    for row in reader:
        print(row)

```

Sample Output:
```
['이름', '나이', '도시']
['김철수', '28', '서울']
['이영희', '34', '부산']
```

## Deep Dive (심층 분석)
CSV 형식은 1970년대부터 사용되어 왔으며 데이터를 표현하는 가장 기본적인 방법 중 하나입니다. JSON, XML과 같은 대안들도 있지만, CSV는 가독성과 단순함 때문에 여전히 많이 사용됩니다. Python의 표준 라이브러리인 `csv` 모듈은 CSV 파일의 생성, 읽기, 쓰기를 도와주며 각 row를 리스트 형태로 처리합니다.

## See Also (더 보기)
- [Python 공식 문서 - csv 모듈](https://docs.python.org/3/library/csv.html)
- [Wikipedia - Comma-separated values](https://en.wikipedia.org/wiki/Comma-separated_values)
- [CSV 읽고 쓰기에 대한 더 심화된 가이드](https://realpython.com/python-csv/)
