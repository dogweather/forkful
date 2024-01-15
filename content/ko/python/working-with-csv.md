---
title:                "CSV 파일을 다루는 방법"
html_title:           "Python: CSV 파일을 다루는 방법"
simple_title:         "CSV 파일을 다루는 방법"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/working-with-csv.md"
---

{{< edit_this_page >}}

## 왜

Python 프로그래밍은 많은 기능과 유연한 사용 방식으로 인해 많은 사람들이 즐겨 사용하는 언어입니다. CSV 파일은 데이터를 쉽게 저장하고 조작할 수 있는 일반적인 파일 형식입니다. 따라서 CSV 파일을 사용해 데이터를 처리하고 가공하는 법을 배우는 것은 굉장히 유용합니다.

## 방법

```Python
import csv

# CSV 파일 열기
with open('data.csv', 'r') as csvfile:
    # csv.reader를 사용해 파일을 읽고 리스트로 저장
    reader = csv.reader(csvfile)
    # 첫 번째 행은 헤더로, 데이터는 두 번째 행부터
    headers = next(reader)
    # 데이터 출력
    for row in reader:
        print(headers[0], row[0])
        print(headers[1], row[1])
```

위 코드는 CSV 파일을 읽고 데이터를 출력하는 간단한 예시입니다. `csv` 모듈을 사용해 파일을 열고 `csv.reader`를 이용해 데이터를 리스트 형태로 저장합니다. 그리고 첫 번째 행은 헤더로 처리하고, 두 번째 행부터 데이터가 저장된 다음 행들을 출력합니다.

```Python
import csv

# CSV 파일 열기
with open('data.csv', 'w') as csvfile:
    # csv.writer를 사용해 데이터를 쓸 준비
    writer = csv.writer(csvfile)
    # 첫 번째 행에 헤더 작성
    writer.writerow(['이름', '성별'])
    # 데이터 쓰기
    writer.writerow(['이지영', '여'])
    writer.writerow(['김진수', '남'])
```

위 코드는 CSV 파일을 만들고 데이터를 쓰는 예시입니다. `csv.writer`를 사용해 파일을 쓸 준비를 하고, 첫 번째 행에는 헤더를 쓰고 그 다음 행부터 데이터를 씁니다. 이렇게 작성된 코드를 실행하면 `data.csv` 파일에 데이터가 쓰여집니다.

## 딥 다이브

CSV 파일을 처리할 때 유용한 몇 가지 기능을 살펴보겠습니다. 첫 번째로 `csv.DictReader`와 `csv.DictWriter`입니다. 이 클래스들은 각각 파일을 딕셔너리 형태로 읽고 쓰는 기능을 제공합니다. 두 번째로 `csv.writer`의 `writerows()` 메소드를 사용하면 여러 행을 한 번에 쓸 수 있습니다. 그리고 `csv` 모듈의 `dialect` 매개변수를 사용해 다양한 CSV 파일 형식을 지원할 수도 있습니다.

## 또 다른 정보

- [파이썬 공식 문서 - csv 모듈](https://docs.python.org/3/library/csv.html)
- [Python으로 CSV 파일 다루기](https://velog.io/@yvvyoon/python-csv-file)