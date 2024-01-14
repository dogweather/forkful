---
title:                "Python: json으로 작업하기"
simple_title:         "json으로 작업하기"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/working-with-json.md"
---

{{< edit_this_page >}}

# 왜 JSON을 다뤄야 할까요?

JSON(JavaScript Object Notation)은 데이터를 교환하는 데 매우 효율적이고 간단한 형식입니다. 이것은 많은 프로그래밍 언어에서 지원되고 있으며, 데이터를 저장하고 전송하는 데 많이 사용됩니다. 따라서 Python 개발자라면 JSON을 다루는 기술은 꼭 알아두어야 합니다.

## 어떻게 하나요?

다음은 Python을 사용하여 JSON 파일을 읽고 쓰는 간단한 예제 코드입니다.

```
import json 

# JSON 파일 읽기
with open("data.json") as f:
    data = json.load(f)

# JSON 파일 쓰기
with open("output.json", "w") as f:
    json.dump(data, f, indent=4)

```

위 코드를 실행하면 `data.json` 파일의 내용이 읽혀서 `data` 변수에 저장되고, `output.json` 파일에 들여쓰기를 포함한 형식으로 저장됩니다.

## 딥 다이브

JSON은 매우 유연하고 다양한 데이터 타입을 지원합니다. 예를 들어 문자열, 숫자, 불리언, 리스트, 딕셔너리 등을 포함할 수 있습니다. 이러한 다양한 데이터 타입에 대한 더 깊은 이해는 더 복잡한 JSON 파일을 다루는 데 매우 유용합니다.

또한, Python에서는 `json` 모듈 외에도 `ujson`, `simplejson` 등 다양한 외부 모듈을 사용하여 더 빠르고 정교한 JSON 다루기를 제공합니다. 이러한 모듈을 사용하면 보다 높은 성능을 얻을 수 있습니다.

## 같이 보기

- [Python 공식 문서 - JSON 모듈](https://docs.python.org/3/library/json.html)
- [Effective JSON with Python](https://realpython.com/python-json/)
- [Python에서 JSON 다루기(번역)](https://medium.com/datadriveninvestor/%ED%8C%8C%EC%9D%B4%EC%8D%AC%EC%97%90%EC%84%9C-json-%EB%8B%A4%EB%A3%A8%EA%B8%B0-d97928cd7f77)