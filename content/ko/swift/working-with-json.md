---
title:                "JSON 다루기"
date:                  2024-01-19
simple_title:         "JSON 다루기"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
(무엇이며, 왜 사용하는가?)
JSON은 데이터를 교환하는 표준 포맷입니다. 간단하고, 가볍고, 인간이 읽기에도, 기계가 파싱하기에도 쉬워서 프로그래머들이 널리 사용합니다.

## How to:
(어떻게 할까?)
Swift에서 JSON 다루기는 `Codable` 프로토콜과 함께 쉽습니다. 아래 예제를 참고하세요.

```Swift
import Foundation

// JSON으로 변환할 구조체 정의
struct User: Codable {
    var name: String
    var age: Int
}

// JSON 문자열
let jsonString = "{\"name\": \"홍길동\", \"age\": 25}"

// JSON 파싱
if let jsonData = jsonString.data(using: .utf8) {
    do {
        let user = try JSONDecoder().decode(User.self, from: jsonData)
        print("\(user.name) 의 나이는 \(user.age)살입니다.")
        // 출력: 홍길동 의 나이는 25살입니다.
    } catch {
        print("오류 발생: \(error)")
    }
}

// 객체를 JSON으로 변환
let user = User(name: "이순신", age: 45)
do {
    let jsonData = try JSONEncoder().encode(user)
    if let jsonString = String(data: jsonData, encoding: .utf8) {
        print(jsonString)
        // 출력: {"name":"이순신","age":45}
    }
} catch {
    print("오류 발생: \(error)")
}
```

## Deep Dive:
(심층 분석)
초기 웹 개발에서는 XML이 데이터 교환의 주요 포맷이었습니다. 하지만 JSON이 등장하면서, 그 편리성 때문에 빠르게 대체되었습니다. Codable은 Swift 4부터 도입되어 JSON의 인코딩과 디코딩을 단순화했습니다. 대안으로는 `JSONSerialization`이 있지만, Codable이 타입 안전성과 사용 편의성에서 뛰어납니다.

## See Also:
(참고 자료)
- Swift 공식 문서 Codable: https://swift.org/documentation/api-design-guidelines/#codable
- JSON 공식 웹사이트: https://www.json.org/json-en.html
- Wikipedia JSON 문서: https://en.wikipedia.org/wiki/JSON
- Apple의 JSON과 작업하는 법: https://developer.apple.com/swift/blog/?id=37
