---
title:                "CSV 처리하기"
html_title:           "Python: CSV 처리하기"
simple_title:         "CSV 처리하기"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/working-with-csv.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

CSV 파일 작업이란 무엇일까요? 이는 Comma-Separated Values의 약자로, 쉼표로 구분된 텍스트 파일을 의미합니다. 프로그래머들은 이를 사용하는 이유는 데이터를 효율적으로 저장하고 불러오기 위해서입니다.

## 방법:

```Python
import csv

# CSV 파일 쓰기 예제
with open('data.csv', 'w', newline='') as csvfile:
    writer = csv.writer(csvfile)
    writer.writerow(['ID', 'Name', 'Age'])
    writer.writerow(['1', 'John', '25'])
    writer.writerow(['2', 'Jane', '29'])

# CSV 파일 읽기 예제
with open('data.csv', newline='') as csvfile:
    reader = csv.reader(csvfile)
    for row in reader:
        print(row)
```
출력:
```
['ID', 'Name', 'Age']
['1', 'John', '25']
['2', 'Jane', '29']
```

## 딥 다이브:

CSV 파일은 1972년 IBM에서 개발되었습니다. 기존에는 포터블 문서를 위해 많이 사용되었지만, 지금은 데이터 저장 및 전송을 위해 널리 사용됩니다. 그 외에도 XML, JSON 등의 다른 데이터 포맷이 있지만, 간단한 데이터를 다룰 때 CSV 파일이 더 유용합니다. Python에서는 csv 모듈을 통해 쉽게 CSV 파일을 다룰 수 있습니다.

## 참고:

- [Python csv 모듈 - 공식 문서](https://docs.python.org/3/library/csv.html)