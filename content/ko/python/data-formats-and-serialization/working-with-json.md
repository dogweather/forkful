---
title:                "JSON과 함께 일하기"
aliases:
- /ko/python/working-with-json.md
date:                  2024-02-03T19:24:34.682835-07:00
model:                 gpt-4-0125-preview
simple_title:         "JSON과 함께 일하기"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇이며 왜인가?

JSON(JavaScript Object Notation)을 사용하는 것은 JSON 형식의 문자열을 파이썬 객체로 파싱하고 그 반대의 과정을 포함합니다. 이는 웹과 API 개발에 있어 핵심적인데, JSON이 서버와 클라이언트 간에 데이터를 교환하는 공용어 역할을 하기 때문입니다.

## 어떻게 사용하나:

파이썬에 내장된 `json` 라이브러리는 인코딩(파이썬 객체를 JSON으로 변환) 및 디코딩(JSON을 파이썬 객체로 변환) 과정을 단순화합니다. 다음은 그 사용 방법입니다:

### 파이썬 객체를 JSON으로 인코딩:

```python
import json

data = {
    "name": "John Doe",
    "age": 30,
    "isEmployee": True,
    "addresses": [
        {"city": "New York", "zipCode": "10001"},
        {"city": "San Francisco", "zipCode": "94016"}
    ]
}

json_string = json.dumps(data, indent=4)
print(json_string)
```

**출력:**

```json
{
    "name": "John Doe",
    "age": 30,
    "isEmployee": true,
    "addresses": [
        {
            "city": "New York",
            "zipCode": "10001"
        },
        {
            "city": "San Francisco",
            "zipCode": "94016"
        }
    ]
}
```

### JSON을 파이썬 객체로 디코딩:

```python
json_string = '''
{
    "name": "John Doe",
    "age": 30,
    "isEmployee": true,
    "addresses": [
        {
            "city": "New York",
            "zipCode": "10001"
        },
        {
            "city": "San Francisco",
            "zipCode": "94016"
        }
    ]
}
'''

data = json.loads(json_string)
print(data)
```

**출력:**

```python
{
    'name': 'John Doe', 
    'age': 30, 
    'isEmployee': True, 
    'addresses': [
        {'city': 'New York', 'zipCode': '10001'}, 
        {'city': 'San Francisco', 'zipCode': '94016'}
    ]
}
```

### 타사 라이브러리 사용하기:

스키마 유효성 검사 또는 URL로부터 직접 JSON 파일을 파싱하는 것과 같은 복잡한 JSON 처리를 위해, `requests` 라이브러리를 HTTP 요청용으로, `jsonschema` 라이브러리를 유효성 검사용으로 사용할 수 있습니다.

#### URL에서 JSON 파싱을 위한 `requests` 예제:

```python
import requests

response = requests.get('https://api.example.com/data')
data = response.json()

print(data)
```

이 스니펫은 주어진 URL로부터 JSON 데이터를 가져와서 직접 파이썬 객체로 변환합니다.

#### `jsonschema`를 사용한 JSON 유효성 검사:

먼저 pip를 통해 라이브러리 설치:

```bash
pip install jsonschema
```

그 다음, 다음과 같이 사용:

```python
from jsonschema import validate
import jsonschema

schema = {
    "type": "object",
    "properties": {
        "name": {"type": "string"},
        "age": {"type": "number"},
        "isEmployee": {"type": "boolean"},
    },
    "required": ["name", "age", "isEmployee"]
}

# JSON 디코딩으로부터 얻은 사전이 `data`라고 가정
try:
    validate(instance=data, schema=schema)
    print("유효한 JSON 데이터입니다.")
except jsonschema.exceptions.ValidationError as err:
    print("유효성 검사 오류:", err)
```

이 예시는 사전으로 정의된 스키마에 따라 디코딩된 JSON 데이터로부터 얻은 파이썬 사전의 유효성을 검사하여 데이터가 기대하는 형식과 유형에 부합하는지를 확인합니다.
