---
title:                "JSON 작업하기"
html_title:           "Python: JSON 작업하기"
simple_title:         "JSON 작업하기"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/working-with-json.md"
---

{{< edit_this_page >}}

# JSON 작업이란 무엇이며 왜 그것을 하는가?

JSON(Javascript Object Notation)은 데이터 교환을 위해 사용되는 경량의 형식입니다. 이는 사람과 컴퓨터 모두가 이해하기 쉽고, 다양한 프로그래밍 언어에서 지원됩니다. 따라서 프로그래머들은 데이터를 효율적으로 교환하기 위해 JSON을 사용합니다.

## 어떻게?

Python에서는 JSON 라이브러리를 사용하여 쉽게 데이터를 읽고 쓸 수 있습니다. 예를 들어, 다음과 같이 사용할 수 있습니다.

```Python
import json

# JSON 데이터 읽기
with open('data.json', 'r') as file:
    data = json.load(file)

# 데이터 추가
data['name'] = 'John'
data['age'] = 25

# JSON 데이터 쓰기
with open('data.json', 'w') as file:
    json.dump(data, file)

# 출력 결과
{
    "name": "John",
    "age": 25,
    "country": "Korea"
}
```

## 깊게 파고들기

JSON은 웹 프로그래밍에서 널리 사용되었으며, Javascript와 밀접한 관련이 있습니다. 하지만 현재는 다양한 프로그래밍 언어에서 지원되고 있으며, XML과 같은 다른 데이터 형식보다 가볍고 효율적입니다. 또한, Python뿐만 아니라 자바, C++, PHP 등 다양한 프로그래밍 언어에서도 JSON을 사용할 수 있습니다.

## 관련 자료

- [JSON 공식 문서](https://www.json.org/json-en.html)
- [Python JSON 라이브러리](https://docs.python.org/3/library/json.html)