---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:16.667881-07:00
description: "Swift\uC5D0\uC11C JSON\uC744 \uB2E4\uB8EC\uB2E4\uB294 \uAC83\uC740 \uB370\
  \uC774\uD130 \uAD50\uD658\uC744 \uC704\uD55C \uACBD\uB7C9\uC758 \uB370\uC774\uD130\
  \ \uD615\uC2DD\uC744 \uB2E4\uB8E8\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 JSON\uC744 \uC11C\uBC84\uC640 \uC6F9\
  \ \uC560\uD50C\uB9AC\uCF00\uC774\uC158 \uC0AC\uC774\uC758 \uB370\uC774\uD130 \uC804\
  \uC1A1\uC744 \uC704\uD574 \uC0AC\uC6A9\uD558\uB294\uB370, \uC774\uB294 \uC778\uAC04\
  \uACFC \uAE30\uACC4 \uBAA8\uB450\uC5D0\uAC8C \uC77D\uAE30 \uC27D\uACE0 \uD30C\uC2F1\
  \uD558\uAE30 \uC27D\uAE30 \uB54C\uBB38\uC785\uB2C8\uB2E4."
lastmod: '2024-03-11T00:14:29.686763-06:00'
model: gpt-4-0125-preview
summary: "Swift\uC5D0\uC11C JSON\uC744 \uB2E4\uB8EC\uB2E4\uB294 \uAC83\uC740 \uB370\
  \uC774\uD130 \uAD50\uD658\uC744 \uC704\uD55C \uACBD\uB7C9\uC758 \uB370\uC774\uD130\
  \ \uD615\uC2DD\uC744 \uB2E4\uB8E8\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 JSON\uC744 \uC11C\uBC84\uC640 \uC6F9\
  \ \uC560\uD50C\uB9AC\uCF00\uC774\uC158 \uC0AC\uC774\uC758 \uB370\uC774\uD130 \uC804\
  \uC1A1\uC744 \uC704\uD574 \uC0AC\uC6A9\uD558\uB294\uB370, \uC774\uB294 \uC778\uAC04\
  \uACFC \uAE30\uACC4 \uBAA8\uB450\uC5D0\uAC8C \uC77D\uAE30 \uC27D\uACE0 \uD30C\uC2F1\
  \uD558\uAE30 \uC27D\uAE30 \uB54C\uBB38\uC785\uB2C8\uB2E4."
title: "JSON\uACFC \uD568\uAED8 \uC77C\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?

Swift에서 JSON을 다룬다는 것은 데이터 교환을 위한 경량의 데이터 형식을 다루는 것을 의미합니다. 프로그래머들은 JSON을 서버와 웹 애플리케이션 사이의 데이터 전송을 위해 사용하는데, 이는 인간과 기계 모두에게 읽기 쉽고 파싱하기 쉽기 때문입니다.

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
