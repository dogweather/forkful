---
title:                "Làm việc với JSON"
date:                  2024-01-28T22:10:39.541489-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với JSON"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/gleam/working-with-json.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Làm việc với JSON (JavaScript Object Notation) tức là xử lý dữ liệu trong một định dạng văn bản phổ biến, dễ hiểu cho cả con người và máy móc. Lập trình viên làm điều này vì JSON là chúa tể trong việc lưu trữ và truyền tải dữ liệu có cấu trúc, đặc biệt là trong các ứng dụng web.

## Làm thế nào:

Dưới đây là cách xử lý JSON trong Gleam bằng cách mã hóa và giải mã dữ liệu. Bạn sẽ cần gói `gleam/json`, vì vậy hãy lấy cái đó trước tiên.

```gleam
import gleam/json

// Định nghĩa một kiểu
pub type Person {
  Person(name: String, age: Int)
}

// Mã hóa thành JSON
pub fn encode_person(person: Person) -> json.Json {
  case person {
    Person(name, age) -> 
      json.object([
        "name", json.string(name),
        "age", json.int(age)
      ])
  }
}
// Cách sử dụng và đầu ra mẫu
let john = Person("John Doe", 30)
let json_john = encode_person(john)
json_john // {"name": "John Doe", "age": 30}

// Giải mã từ JSON
pub fn decode_person(json: json.Json) -> Result(Person, Nil) {
  let Ok(json) = json.decode_pair() // Giải mã đối tượng JSON
  let Ok(name) = json.field("name").map(json.decode_string)
  let Ok(age) = json.field("age").map(json.decode_int)
  person.Person(name, age)
}
// Cách sử dụng và đầu ra mẫu
let decoded_person = decode_person(json_object("{\"name\": \"John Doe\", \"age\": 30}"))
decoded_person // Ok(Person("John Doe", 30))
```

## Đào sâu

JSON đã xuất hiện từ đầu những năm 2000, thay thế XML trong nhiều tình huống bằng sự đơn giản của nó. Một số lựa chọn thay thế bao gồm YAML, XML và BSON, trong số những người khác, nhưng sự dễ sử dụng của nó giữ JSON ở vị trí hàng đầu. Trong Gleam, việc xử lý JSON dựa trên việc khớp mẫu và các chức năng mạnh mẽ của thư viện `gleam/json` cho một cách tiếp cận chức năng để mã hóa và giải mã các cấu trúc dữ liệu.

## Xem thêm

- Tài liệu chính thức về JSON của Gleam: [https://hexdocs.pm/gleam_json](https://hexdocs.pm/gleam_json)
- Giới thiệu về JSON: [https://www.json.org/json-en.html](https://www.json.org/json-en.html)
- Hướng dẫn về JSON của Mozilla Developer Network: [https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Objects/JSON](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Objects/JSON)
