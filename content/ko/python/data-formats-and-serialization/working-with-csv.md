---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:07.526194-07:00
description: "\uBC29\uBC95: Python\uC740 \uB0B4\uC7A5\uB41C `csv` \uBAA8\uB4C8\uC744\
  \ \uC81C\uACF5\uD558\uC5EC CSV \uD30C\uC77C\uC744 \uB2E4\uB8E8\uB294 \uAC83\uC744\
  \ \uAC04\uB2E8\uD558\uAC8C \uD574\uC8FC\uC5B4, \uC774\uB97C \uD1B5\uD574 \uD30C\uC77C\
  \uB85C\uBD80\uD130 \uC77D\uACE0 \uC4F0\uAE30\uAC00 \uC26C\uC6CC\uC9D1\uB2C8\uB2E4\
  . \uB354 \uACAC\uACE0\uD558\uACE0 \uBCF5\uC7A1\uD55C \uB370\uC774\uD130 \uC870\uC791\
  \uC744 \uC704\uD574, \uC11C\uB4DC \uD30C\uD2F0 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uC778\
  \ `pandas`\uAC00 \uB9E4\uC6B0 \uC778\uAE30\uAC00 \uB192\uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.632177-06:00'
model: gpt-4-0125-preview
summary: "Python\uC740 \uB0B4\uC7A5\uB41C `csv` \uBAA8\uB4C8\uC744 \uC81C\uACF5\uD558\
  \uC5EC CSV \uD30C\uC77C\uC744 \uB2E4\uB8E8\uB294 \uAC83\uC744 \uAC04\uB2E8\uD558\
  \uAC8C \uD574\uC8FC\uC5B4, \uC774\uB97C \uD1B5\uD574 \uD30C\uC77C\uB85C\uBD80\uD130\
  \ \uC77D\uACE0 \uC4F0\uAE30\uAC00 \uC26C\uC6CC\uC9D1\uB2C8\uB2E4."
title: "CSV\uC640 \uD568\uAED8 \uC791\uC5C5\uD558\uAE30"
weight: 37
---

## 방법:
Python은 내장된 `csv` 모듈을 제공하여 CSV 파일을 다루는 것을 간단하게 해주어, 이를 통해 파일로부터 읽고 쓰기가 쉬워집니다. 더 견고하고 복잡한 데이터 조작을 위해, 서드 파티 라이브러리인 `pandas`가 매우 인기가 높습니다.

### `csv` 모듈 사용하기


#### CSV 파일 읽기
```python
import csv

with open('sample.csv', mode='r') as file:
    csv_reader = csv.reader(file)
    for row in csv_reader:
        print(row)
```
*`sample.csv`에 다음과 같은 내용이 있다고 가정:*
```
name,age,city
John,22,New York
Jane,28,Los Angeles
```
*출력:*
```
['name', 'age', 'city']
['John', '22', 'New York']
['Jane', '28', 'Los Angeles']
```

#### CSV 파일 쓰기
```python
import csv

rows = [['name', 'age', 'city'], ['Jack', '33', 'Chicago'], ['Emily', '41', 'Denver']]

with open('output.csv', mode='w', newline='') as file:
    writer = csv.writer(file)
    writer.writerows(rows)
```
*`output.csv`를 다음으로 생성하거나 덮어씁니다:*
```
name,age,city
Jack,33,Chicago
Emily,41,Denver
```

### `pandas`로 CSV 다루기
`pandas`는 데이터 조작을 단순화하는 강력한 라이브러리로 다른 데이터 형식들 사이에서도 CSV 파일을 다루는 것을 간편하게 합니다.

#### pandas 설치
```shell
pip install pandas
```

#### pandas로 CSV 파일 읽기
```python
import pandas as pd

df = pd.read_csv('sample.csv')
print(df)
```
*출력:*
```
    name  age         city
0   John   22    New York
1   Jane   28  Los Angeles
```

#### pandas로 CSV 파일 쓰기
```python
import pandas as pd

df = pd.DataFrame({'name': ['Jack', 'Emily'], 'age': [33, 41], 'city': ['Chicago', 'Denver']})
df.to_csv('output_pandas.csv', index=False)
```
*`output_pandas.csv`를 다음으로 생성하거나 덮어씁니다:*
```
name,age,city
Jack,33,Chicago
Emily,41,Denver
```
