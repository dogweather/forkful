---
title:                "JSON 다루기"
html_title:           "Arduino: JSON 다루기"
simple_title:         "JSON 다루기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜?)
JSON은 데이터 교환 형식입니다. 프로그래머들이 다양한 언어와 플랫폼에서 데이터를 공유하고 구조화된 데이터를 쉽게 처리할 수 있게 해줍니다.

## How to: (방법)
```gleam
import gleam/json
import gleam/map

fn main() {
  // JSON 문자열 파싱
  let json_str = "{\"name\":\"John\", \"age\":30, \"city\":\"New York\"}"
  let json_value = json.decode(json_str)
  
  // JSON 값을 Map으로 변환
  case json_value {
    Ok(value) -> 
      let data = json.from(value)
      case data {
        map.Map(items) -> 
          items
            |> map.to_list
            |> list.map(fn(tuple) {
              let (key, value) = tuple
              json.to_string(value)
            })
            |> io.debug
        _ -> io.debug("Not a JSON Object")
      }
    Error(_) ->
      io.debug("Failed to parse JSON")
  }
}
```

Sample output:
```
["\"John\"", "30", "\"New York\""]
```

## Deep Dive (깊이 알아보기)
JSON은 JavaScript Object Notation의 약자로, 웹의 성장과 함께 표준 데이터 포맷으로 자리잡았습니다. XML과 비교하여 간결함이 주요 장점입니다. Gleam에서는 `gleam/json` 모듈을 사용하여 JSON 데이터를 처리합니다. 이 모듈은 JSON값을 Gleam 타입으로 바꾸는 데 필요한 함수들을 제공합니다.

## See Also (참고 자료)
- JSON tutorial: [https://www.json.org/json-en.html](https://www.json.org/json-en.html)
- Comparison of JSON and XML: [https://en.wikipedia.org/wiki/JSON#Comparison_with_XML](https://en.wikipedia.org/wiki/JSON#Comparison_with_XML)