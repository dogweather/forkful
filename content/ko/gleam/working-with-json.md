---
title:                "json 작업"
html_title:           "Gleam: json 작업"
simple_title:         "json 작업"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON(JavaScript Object Notation)을 다루는 것은 데이터를 교환하고 저장하는 데에 매우 유용합니다. JSON은 기본적으로 속성-값 쌍으로 이루어진 데이터 형식이며, 많은 프로그래밍 언어에서 지원하고 있습니다. 따라서, 프로그래머들은 자주 JSON을 사용하여 데이터를 전달하고 저장하는데 이용합니다.

## How to:
다음은 Gleam에서 JSON을 다루는 예제 코드입니다.
```Gleam
import gleam/json

let user = json.encode_user({name: "John", age: 30})
gleam.log(user.name) // 출력 결과: "John"
```
위의 코드는 `gleam/json` 모듈을 이용하여 JSON 포맷으로 유저 정보를 인코딩하고, 해당 정보를 출력하는 예제입니다.

## Deep Dive:
JSON은 2000년대 초에 더글러스 크록포드(Douglas Crockford)에 의해 만들어진 데이터 형식입니다. 이전에는 XML과 같은 다른 데이터 형식들이 더 널리 사용되었지만, JSON은 더 간단하고 사용하기 쉬운 형식으로 인기를 얻게 되었습니다. 현재까지 JSON은 데이터 교환 및 저장을 위한 가장 인기있는 형식 중 하나로 남아있습니다. 또한, 대부분의 프로그래밍 언어에서 지원하는 것도 이러한 인기에 큰 역할을 합니다.

## See Also:
더 많은 정보를 원하신다면, 다음 링크들을 참고하세요.
- [JSON.org](https://www.json.org/): JSON 공식 웹사이트
- [Gleam 공식 문서](https://gleam.run/): Gleam 언어 공식 문서