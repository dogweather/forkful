---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:07.526194-07:00
description: "CSV(\uC27C\uD45C\uB85C \uAD6C\uBD84\uB41C \uAC12)\uB97C \uB2E4\uB8E8\
  \uB294 \uAC83\uC740 CSV \uD30C\uC77C\uC5D0\uC11C \uB370\uC774\uD130\uB97C \uC77D\
  \uACE0 \uC4F0\uB294 \uC791\uC5C5\uC744 \uD3EC\uD568\uD558\uBA70, \uC774\uB294 \uD45C\
  \ \uB370\uC774\uD130\uB97C \uC800\uC7A5\uD558\uAE30 \uC704\uD55C \uC77C\uBC18\uC801\
  \uC778 \uD615\uC2DD\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740\
  \ \uB2E4\uC591\uD55C \uD50C\uB7AB\uD3FC\uACFC \uC5B8\uC5B4\uC5D0\uC11C \uB110\uB9AC\
  \ \uC9C0\uC6D0\uB418\uB294 \uAC04\uB2E8\uD55C \uD14D\uC2A4\uD2B8 \uAE30\uBC18 \uD615\
  \uC2DD\uC73C\uB85C \uB370\uC774\uD130\uB97C \uC27D\uAC8C \uAD50\uD658\uD558\uACE0\
  \ \uC800\uC7A5\uD558\uAE30 \uC704\uD574 \uC774\u2026"
lastmod: 2024-02-19 22:05:13.573574
model: gpt-4-0125-preview
summary: "CSV(\uC27C\uD45C\uB85C \uAD6C\uBD84\uB41C \uAC12)\uB97C \uB2E4\uB8E8\uB294\
  \ \uAC83\uC740 CSV \uD30C\uC77C\uC5D0\uC11C \uB370\uC774\uD130\uB97C \uC77D\uACE0\
  \ \uC4F0\uB294 \uC791\uC5C5\uC744 \uD3EC\uD568\uD558\uBA70, \uC774\uB294 \uD45C\
  \ \uB370\uC774\uD130\uB97C \uC800\uC7A5\uD558\uAE30 \uC704\uD55C \uC77C\uBC18\uC801\
  \uC778 \uD615\uC2DD\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740\
  \ \uB2E4\uC591\uD55C \uD50C\uB7AB\uD3FC\uACFC \uC5B8\uC5B4\uC5D0\uC11C \uB110\uB9AC\
  \ \uC9C0\uC6D0\uB418\uB294 \uAC04\uB2E8\uD55C \uD14D\uC2A4\uD2B8 \uAE30\uBC18 \uD615\
  \uC2DD\uC73C\uB85C \uB370\uC774\uD130\uB97C \uC27D\uAC8C \uAD50\uD658\uD558\uACE0\
  \ \uC800\uC7A5\uD558\uAE30 \uC704\uD574 \uC774\u2026"
title: "CSV\uC640 \uD568\uAED8 \uC791\uC5C5\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇을, 왜?
CSV(쉼표로 구분된 값)를 다루는 것은 CSV 파일에서 데이터를 읽고 쓰는 작업을 포함하며, 이는 표 데이터를 저장하기 위한 일반적인 형식입니다. 프로그래머들은 다양한 플랫폼과 언어에서 널리 지원되는 간단한 텍스트 기반 형식으로 데이터를 쉽게 교환하고 저장하기 위해 이 작업을 합니다.

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
