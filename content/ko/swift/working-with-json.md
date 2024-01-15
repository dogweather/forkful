---
title:                "json 사용하기"
html_title:           "Swift: json 사용하기"
simple_title:         "json 사용하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/working-with-json.md"
---

{{< edit_this_page >}}

## 왜 JSON을 사용해야 할까요?

JSON은 현대 웹 프로그래밍에서 매우 중요한 데이터 포맷입니다. 웹 애플리케이션을 개발하거나 서버와 클라이언트 간의 통신을 할 때, 데이터를 쉽고 간결하게 표현하고 전송하기 위해 JSON을 사용합니다.

## JSON 사용하는 방법

```Swift
// 샘플 JSON 데이터
let json = """
{
    "name": "John",
    "age": 30,
    "hobbies": [
        "reading",
        "coding",
        "hiking"
    ]
}
""".data(using: .utf8)!

// JSON 디코딩하여 데이터 구조체에 할당
struct Person: Codable {
    var name: String
    var age: Int
    var hobbies: [String]
}

// JSON 파싱 및 데이터 출력
let decoder = JSONDecoder()
if let person = try? decoder.decode(Person.self, from: json) {
    print("이름: \(person.name)")
    print("나이: \(person.age)")
    print("취미: \(person.hobbies)")
}
```

출력 결과:
```
이름: John
나이: 30
취미: ["reading", "coding", "hiking"]
```

## JSON의 깊은 이해

JSON은 JavaScript Object Notation의 약자로, 자바스크립트 객체를 표현하기 위해 만들어졌지만 이제는 다양한 프로그래밍 언어에서 사용됩니다. JSON은 바이너리 데이터보다는 텍스트 데이터를 다루고, 데이터 내에 어떤 종류의 데이터도 표현할 수 있습니다. 또한, 배열과 같은 자료구조를 지원해 데이터를 구조적으로 표현할 수 있습니다.

## 더 알아보기

[Apple Developer Documentation for JSONSerialization](https://developer.apple.com/documentation/foundation/jsonserialization)
[Ray Wenderlich's Working with JSON in Swift Tutorial](https://www.raywenderlich.com/5492-working-with-json-in-swift)
[SwiftLee's How to Work with JSON in Swift 5](https://www.avanderlee.com/swift/json-parsing-decoding/)