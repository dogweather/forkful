---
title:                "JSON 처리하기"
html_title:           "Python: JSON 처리하기"
simple_title:         "JSON 처리하기"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/working-with-json.md"
---

{{< edit_this_page >}}

## 왜
JSON은 프로그래밍에서 데이터를 교환하는 데 널리 사용되기 때문에, Python 프로그래머에게 중요한 개념입니다. 다양한 API와 서버에서 JSON 형식으로 데이터를 전송하고 받는 일이 많기 때문에, JSON을 다루는 기술은 중요합니다.

## 사용 방법
JSON 데이터를 파이썬에서 다루는 방법은 간단합니다. 우선, `json` 라이브러리를 `import` 해줍니다.

```python
import json
```

`json` 라이브러리는 `load()`와 `dump()` 함수를 제공합니다. `load()` 함수는 JSON 파일을 읽어 파이썬에서 사용할 수 있는 데이터로 변환해줍니다. 반대로, `dump()` 함수는 파이썬 데이터를 JSON 형식으로 변환하여 파일로 저장해줍니다.

```python
# JSON 파일 읽어오기 (load 함수)
with open('example.json', 'r') as f:
    data = json.load(f)
# 파이썬 데이터를 JSON 형식으로 저장하기 (dump 함수)
with open('example.json', 'w') as f:
    json.dump(data, f)
```

JSON 데이터는 `key`와 `value`로 이루어져 있습니다. 파이썬에서는 `Dictionary` 자료형과 비슷한 형식으로 JSON 데이터를 다룰 수 있습니다.

```python
data = {"name": "John", "age": 25, "address": "Seoul"}
# JSON으로 변환
json_data = json.dumps(data)
print(json_data) # {"name": "John", "age": 25, "address": "Seoul"}
# JSON 데이터를 파이썬으로 변환
python_data = json.loads(json_data)
print(python_data) # {"name": "John", "age": 25, "address": "Seoul"}
```

## 더 깊이 파헤치기
JSON 데이터는 여러 가지 형태로 구성될 수 있습니다. 배열, 객체, 값을 가지는 `key` 등 다양한 형태를 가질 수 있습니다. 따라서, `json` 라이브러리의 `loads()` 함수를 사용할 때 변환된 데이터의 형태를 반드시 확인해야 합니다. 또한, `key`를 통해 해당하는 값을 찾을 수 있으므로, 데이터의 구조를 잘 파악하고 사용해야 합니다.

## 관련 자료
[Python json 라이브러리 공식 문서](https://docs.python.org/3/library/json.html)

[JSON Overview from w3schools](https://www.w3schools.com/js/js_json_intro.asp)

[Python and JSON: Working with Large Data Sets in Python](https://realpython.com/python-json/)