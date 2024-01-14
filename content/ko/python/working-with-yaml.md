---
title:                "Python: yaml과 함께 작업하기"
simple_title:         "yaml과 함께 작업하기"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## 왜

YAML은 Python 프로그래밍에서 굉장히 중요한 역할을 합니다. YAML 파일을 다룰 수 있으면, 데이터를 더 쉽게 관리하고 코드를 더욱 효율적으로 작성할 수 있습니다. 따라서, YAML을 사용하는 것은 파이썬 프로그래머에게 꼭 필요한 스킬입니다.

## 어떻게

YAML을 사용하는 방법은 매우 간단합니다. 먼저, YAML 라이브러리를 import 하고, 파일을 읽고 쓰는 방법을 알아야 합니다.

```
# YAML 라이브러리 import
import yaml

# YAML 파일 읽기
with open("example.yml", "r") as file:
    data = yaml.load(file, Loader=yaml.FullLoader)
print(data)

# YAML 파일 쓰기
data = {'name': 'John', 'age': 30, 'city': 'Seoul'}
with open("new_file.yml", "w") as file:
    yaml.dump(data, file)
```

출력:

```
{'name': 'John', 'age': 30, 'city': 'Seoul'}
```

## 깊이 파헤치기

YAML 파일은 파이썬의 딕셔너리와 매우 유사합니다. 따라서, 데이터를 관리하기 쉽고 가독성이 높은 형식으로 저장할 수 있습니다. 또한, YAML 파일은 JSON 파일보다 더 다양한 데이터 타입을 지원하며, 주석을 추가할 수도 있습니다.

예를 들어, YAML 파일에서 리스트를 사용하는 방법은 다음과 같습니다.

```
heroes:
  - name: Batman
    power: intelligence
  - name: Spiderman
    power: agility
```

출력:

```
{'heroes': [{'name': 'Batman', 'power': 'intelligence'}, {'name': 'Spiderman', 'power': 'agility'}]}
```

더 많은 정보와 예제는 [공식 문서](https://yaml.org/)를 참고하시기 바랍니다.

## 관련 링크

- [YAML 공식 문서](https://yaml.org/)
- [Python YAML 라이브러리 문서](https://pyyaml.org/)