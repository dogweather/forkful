---
title:                "Gleam: Json 작업하기"
simple_title:         "Json 작업하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/working-with-json.md"
---

{{< edit_this_page >}}

## 왜

JSON은 현재 가장 널리 사용되는 데이터 형식 중 하나이며, 우리는 많은 곳에서 이를 볼 수 있습니다. Gleam은 이러한 JSON 데이터를 다루는 데 매우 효율적인 방법을 제공하며, 이를 활용하면 데이터를 효율적으로 처리하고 쉽게 구조화할 수 있습니다.

## 방법

Gleam에서 JSON을 다루기 위해서는 ```src/gleam/external/json``` 패키지를 임포트해야 합니다. 이 패키지는 JSON 데이터를 파싱하고 빌드하는데 필요한 모든 도구들을 제공합니다. 아래는 Gleam에서 JSON 데이터를 파싱하는 간단한 예제입니다.

```Gleam
import gleam/external/json

let person = json.parse_string(
  """
  {
    "name": "John",
    "age": 30,
    "address": {
      "street": "123 Main St.",
      "city": "Seoul"
    }
  }
  """
)

let name = person["name"] // "John"
let age = person["age"] // 30
let address = person["address"] // {"street": "123 Main St.", "city": "Seoul"}
```

JSON 데이터를 다루는 Gleam의 다른 기능으로는 데이터를 구조화하거나 필터링할 수 있도록 하는 함수들이 있습니다. 아래는 이러한 함수들을 사용하여 위 예제에서 가져온 데이터를 구조화하고 필터링하는 예제입니다.

```Gleam
let person = json.parse_string(
  """
  [
    {
      "name": "John",
      "age": 30,
      "address": {
        "street": "123 Main St.",
        "city": "Seoul"
      }
    },
    {
      "name": "Jane",
      "age": 25,
      "address": {
        "street": "456 Broadway",
        "city": "Busan"
      }
    }
  ]
  """
)

let names = person["name"] // ["John", "Jane"]
let city = person["address"]["city"] // ["Seoul", "Busan"]

let over_30 = person["age"] |> json.filter_ints(i -> i > 30) // [30]
```

## 깊게 파고들기

Gleam에서는 JSON 데이터를 다루는 것 외에도 고급적인 기능들을 제공합니다. 예를 들어, 다른 데이터 타입과 마찬가지로 패턴 매칭을 통해 JSON 데이터를 처리할 수 있습니다. 또한, JSON 스키마를 사용하여 데이터 타입을 정의하고 직렬화된 데이터를 Gleam 값으로 변환할 수도 있습니다.

더 깊이 들어가려면 Gleam 공식 문서에서 JSON 관련 기능들을 살펴보세요!

## 참고자료

- [Gleam 공식 문서](https://gleam.run/)