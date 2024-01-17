---
title:                "json으로 작업하기"
html_title:           "Swift: json으로 작업하기"
simple_title:         "json으로 작업하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/working-with-json.md"
---

{{< edit_this_page >}}

## 🤔 무엇이고 왜? 

JSON 작업이란 무엇일까요? 간단하게 말해, JSON은 데이터 교환을 위한 텍스트 형식입니다. 프로그래머들은 이를 사용하여 서로 다른 시스템 간에 데이터를 교환하고 공유할 수 있습니다. 스위프트 프로그래밍에서는 JSON을 사용하여 웹 서비스에서 데이터를 로드하거나 전송하는 등 다양한 용도로 활용할 수 있습니다.

## 💻 어떻게 하나요?

아래 코드 블록에서는 JSON 데이터를 가져와서 특정 구조체를 생성하는 간단한 예제를 보여드리겠습니다.

```swift
let data = """
{
    "name": "John",
    "age": 30,
    "occupation": "Developer"
}
""".data(using: .utf8)!

struct Person: Codable {
    let name: String
    let age: Int
    let occupation: String
}

let decoder = JSONDecoder()
let person = try decoder.decode(Person.self, from: data)
print(person.name)
// Output: John
```

## 📚 Deep Dive

JSON은 현재 가장 일반적으로 사용되는 데이터 형식 중 하나입니다. 하지만 이전에는 XML과 같은 다른 형식이 사용되던 시대도 있었습니다. 스위프트에서 JSON을 다루는 방법은 `Codable` 프로토콜을 구현하는 것으로 매우 간단해졌습니다. 그 외에도 `SwiftyJSON`과 같은 라이브러리를 사용하여 더욱 편리하고 효율적으로 JSON을 다룰 수 있습니다.

## 🔍 참고 자료

- [Swift.org - JSON](https://swift.org/documentation/api-design-guidelines/#casing)
- [SwiftyJSON on GitHub](https://github.com/SwiftyJSON/SwiftyJSON)
- [Codable Protocols on Ray Wenderlich](https://www.raywenderlich.com/3418439-encoding-and-decoding-in-swift-with-codable)