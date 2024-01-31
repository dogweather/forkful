---
title:                "JSON 다루기"
date:                  2024-01-19
simple_title:         "JSON 다루기"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON(JavaScript Object Notation)은 데이터를 저장하고 전송할 때 쓰는 경량 텍스트 데이터 포맷입니다. 프로그래머는 이 포맷을 사용하여 다양한 언어와 플랫폼 간에 데이터를 쉽게 교환하고 저장할 수 있습니다.

## How to:

### JSON 데이터 읽기
```Python
import json

# JSON 문자열
json_data = '{"name": "Kim", "age": 30, "city": "Seoul"}'

# 파이썬 객체로 변환
python_dict = json.loads(json_data)

print(python_dict)
```
출력:
```Python
{'name': 'Kim', 'age': 30, 'city': 'Seoul'}
```

### JSON 파일 읽고 쓰기
```Python
import json

# 파일로 부터 JSON 데이터 읽기
with open('data.json', 'r', encoding='utf-8') as f:
    data = json.load(f)

# data 처리...

# 파이썬 객체를 JSON 파일로 쓰기
with open('output.json', 'w', encoding='utf-8') as f:
    json.dump(data, f, ensure_ascii=False, indent=4)
```

## Deep Dive

JSON은 2000년대 초반 Douglas Crockford에 의해 개발되었으며, XML의 대안으로 빠르게 자리 잡았습니다. XML보다 가볍고, 구문이 간단하기 때문에 많은 프로그래머들이 선호하는 데이터 교환 포맷입니다. 파이썬의 `json` 모듈은 RFC 7159에 기반하여 구현되었으며, `load()`와 `dump()` 메소드는 각각 파일에서 데이터를 읽고 쓰기 위해 사용됩니다. 유사한 라이브러리로는 `simplejson`, `ujson`, `orjson` 등이 있습니다.

## See Also

- 파이썬 공식 json 모듈 문서: https://docs.python.org/3/library/json.html
- JSON 공식 웹사이트: https://www.json.org/json-en.html
- Douglas Crockford의 JSON에 대한 개요: https://www.crockford.com/json.html
