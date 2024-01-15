---
title:                "yaml로 작업하기"
html_title:           "Python: yaml로 작업하기"
simple_title:         "yaml로 작업하기"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## 왜 <br>
당신은 YAML을 사용하여 작업을 하게 될까요? YAML은 데이터를 저장하기에 매우 효율적이고 보기 쉬운 형식을 가지고 있기 때문입니다. 또한, 다른 형식으로 변환하기 쉽고 컴퓨터가 읽고 해석하기도 간단하기 때문에 많은 개발자들이 이를 선호하게 됩니다.

## 사용 방법 <br>

```Python
import yaml

# YAML 파일 읽기
with open('data.yml', 'r') as file:
    data = yaml.load(file, Loader=yaml.FullLoader)

# 데이터 변경 및 저장
data['name'] = 'John'
data['age'] = 25

with open('new_data.yml', 'w') as file:
    yaml.dump(data, file)

# YAML을 딕셔너리로 변환
dict_data = yaml.safe_load('data.yml')

# 딕셔너리를 YAML로 변환
yaml_data = yaml.safe_dump(dict_data)
```

출력:
```
name: John
age: 25
```

## 깊게 파헤치기 <br>
YAML은 대부분의 코딩 작업에서 사용할 수 있습니다. 파일 경로, 세션 설정, 데이터베이스 구성 등 다양한 정보를 저장할 수 있으며 여러 언어로 쉽게 변환할 수 있기 때문에 매우 편리합니다. 또한, YAML은 인간이 읽고 이해하기 쉬운 형식을 가지고 있기 때문에 협업 과정에서도 유용하게 사용될 수 있습니다.

## 더 많은 정보 보기 <br>
- YAML 공식 문서: https://yaml.org/
- Python 공식 document: https://docs.python.org/3/library/yaml.html
- YAML 사용 예제: https://realpython.com/python-yaml/
- YAML 파일 데이터 읽고 쓰기: https://www.geeksforgeeks.org/reading-writing-yaml-files-python/
- YAML을 딕셔너리로 변환하기: https://stackoverflow.com/questions/52692871/how-to-convert-yaml-to-dictionary-in-python