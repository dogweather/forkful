---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:16.667881-07:00
description: "\uC5B4\uB5BB\uAC8C \uD558\uB098: Swift\uB294 `Codable` \uD504\uB85C\uD1A0\
  \uCF5C\uC744 \uD1B5\uD574 JSON \uD30C\uC2F1\uC744 \uAC04\uB2E8\uD558\uAC8C \uB9CC\
  \uB4ED\uB2C8\uB2E4. \uB2E4\uC74C\uC740 JSON\uC744 Swift \uAC1D\uCCB4\uB85C \uB514\
  \uCF54\uB4DC\uD558\uB294 \uBC29\uBC95\uC785\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.762219-06:00'
model: gpt-4-0125-preview
summary: "Swift\uB294 `Codable` \uD504\uB85C\uD1A0\uCF5C\uC744 \uD1B5\uD574 JSON \uD30C\
  \uC2F1\uC744 \uAC04\uB2E8\uD558\uAC8C \uB9CC\uB4ED\uB2C8\uB2E4."
title: "JSON\uACFC \uD568\uAED8 \uC77C\uD558\uAE30"
weight: 38
---

## 어떻게 하나:
Swift는 `Codable` 프로토콜을 통해 JSON 파싱을 간단하게 만듭니다. 다음은 JSON을 Swift 객체로 디코드하는 방법입니다:

```Swift
import Foundation

// Codable을 준수하는 모델 정의
struct User: Codable {
    var name: String
    var age: Int
}

// JSON 문자열
let jsonString = """
{
    "name": "John Doe",
    "age": 30
}
"""

// JSON 문자열을 Data로 변환
if let jsonData = jsonString.data(using: .utf8) {
    // JSON 데이터를 User 객체로 디코드
    do {
        let user = try JSONDecoder().decode(User.self, from: jsonData)
        print("이름: \(user.name), 나이: \(user.age)")
    } catch {
        print("JSON 디코딩 오류: \(error)")
    }
}
```

샘플 출력:
```
이름: John Doe, 나이: 30
```

## 심층 분석
JSON(JavaScript Object Notation)은 Douglas Crockford가 명시한 이후 2000년대 초반부터 광범위하게 채택되었습니다. 더 간단한 문법과 더 나은 성능 때문에 많은 사용 사례에서 XML을 대체했습니다. Swift의 `Codable`이 JSON을 다루기 위한 가장 선호되는 방법이지만, `JSONSerialization`과 같은 대안들이 `Codable`-호환되지 않는 타입을 다룰 때 존재합니다. 내부적으로, `Codable`은 더 낮은 수준의 파싱을 추상화하고 직렬화/역직렬화를 매끄럽게 만듭니다.

## 참고
- 공식 Swift 블로그에서 JSON 및 Swift에 대해 더 알아보세요: [Swift.org](https://swift.org/blog/)
- `Codable` 문서를 확인해 보세요: [Swift Codable](https://developer.apple.com/documentation/swift/codable)
- 복잡한 JSON 구조체를 다루려면 [GitHub](https://github.com/SwiftyJSON/SwiftyJSON)에서 제공하는 SwiftyJSON과 같은 제3자 라이브러리를 고려하세요.
