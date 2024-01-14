---
title:                "Python: CSV로 작업하기"
simple_title:         "CSV로 작업하기"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/working-with-csv.md"
---

{{< edit_this_page >}}

## 왜

CSV 파일은 데이터를 저장하고 공유하기에 매우 유용한 형식입니다. 파이썬에서도 CSV 파일을 다루는 것이 쉽고 간단하여, 데이터 분석이나 데이터베이스 작업에 이용할 수 있습니다.

## 어떻게

CSV 파일은 콤마로 구분된 문자열을 포함하는 텍스트 파일입니다. 파이썬에서는 `csv` 모듈을 이용하여 파일을 읽고 쓸 수 있습니다. 예를 들어, `example.csv` 파일에 다음과 같은 데이터가 있다고 가정해보겠습니다.

```
Name, Age, City
John, 30, New York
Jane, 25, Seoul
Tom, 28, London
```

우리는 이 파일을 다음과 같이 파이썬 코드로 읽을 수 있습니다.

```
import csv

with open("example.csv") as file:
    csv_reader = csv.reader(file, delimiter=",")

    for row in csv_reader:
        print(row)
```

위 코드는 파일을 읽어 콤마로 구분된 데이터를 각각의 리스트로 반환합니다. 즉, 출력은 다음과 같이 될 것입니다.

```
["Name", "Age", "City"]
["John", "30", "New York"]
["Jane", "25", "Seoul"]
["Tom", "28", "London"]
```

만약 첫 번째 줄을 제외하고 싶다면 `next()` 함수를 이용할 수 있습니다.

```
import csv

with open("example.csv") as file:
    csv_reader = csv.reader(file, delimiter=",")

    # 첫 번째 줄 제외
    next(csv_reader)

    for row in csv_reader:
        print(row)
```

출력 결과는 다음과 같이 첫 번째 줄을 제외한 나머지 줄이 출력됩니다.

```
["John", "30", "New York"]
["Jane", "25", "Seoul"]
["Tom", "28", "London"]
```

파일에 데이터를 추가하려면 `csv.writer()` 함수를 이용하면 됩니다. 예를 들어, `new_users.csv` 파일에 새로운 사용자 정보를 추가하고 싶다면 다음과 같이 할 수 있습니다.

```
import csv

with open("new_users.csv", mode="a") as file:
    csv_writer = csv.writer(file, delimiter=",")

    # 새로운 데이터 추가
    new_user = ["Mike", "32", "Berlin"]
    csv_writer.writerow(new_user)
```

## 더 들어가보기

CSV 파일을 다루는 더 많은 기능에 대해서는 `csv` 모듈의 [문서](https://docs.python.org/3/library/csv.html)를 참고하시기 바랍니다. CSV 파일이나 작업을 할 때 유용한 다른 파이썬 모듈은 `pandas`나 `numpy` 등이 있습니다.

## 관련 링크

- [파이썬 공식 `csv` 모듈 문서](https://docs.python.org/3/library/csv.html)
- [`pandas` 모듈 공식 문서](https://pandas.pydata.org/docs/)
- [`numpy` 모듈 공식 문서](https://numpy.org/doc/)