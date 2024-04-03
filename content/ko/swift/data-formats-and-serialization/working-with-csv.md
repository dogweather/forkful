---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:34.346087-07:00
description: "\uBC29\uBC95: Swift\uC5D0\uC11C\uB294 CSV \uD30C\uC77C\uC744 \uC9C1\uC811\
  \ \uD30C\uC2F1\uD558\uAE30 \uC704\uD55C \uB124\uC774\uD2F0\uBE0C \uC9C0\uC6D0\uC774\
  \ \uC5C6\uC9C0\uB9CC, `String` \uBA54\uC18C\uB4DC\uB97C \uC0AC\uC6A9\uD558\uC5EC\
  \ \uB0B4\uC6A9\uC744 \uBD84\uD560\uD558\uAC70\uB098 SwiftCSV\uC640 \uAC19\uC740\
  \ \uD0C0\uC0AC \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uD65C\uC6A9\uD558\uC5EC \uB354\
  \ \uAC04\uD3B8\uD55C \uBC29\uC2DD\uC73C\uB85C CSV \uB370\uC774\uD130\uB97C \uCC98\
  \uB9AC\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uB2E4\uC74C\uC740 \uB450 \uAC00\uC9C0\
  \ \uBC29\uBC95\uC785\uB2C8\uB2E4: #."
lastmod: '2024-03-13T22:44:55.763821-06:00'
model: gpt-4-0125-preview
summary: "Swift\uC5D0\uC11C\uB294 CSV \uD30C\uC77C\uC744 \uC9C1\uC811 \uD30C\uC2F1\
  \uD558\uAE30 \uC704\uD55C \uB124\uC774\uD2F0\uBE0C \uC9C0\uC6D0\uC774 \uC5C6\uC9C0\
  \uB9CC, `String` \uBA54\uC18C\uB4DC\uB97C \uC0AC\uC6A9\uD558\uC5EC \uB0B4\uC6A9\uC744\
  \ \uBD84\uD560\uD558\uAC70\uB098 SwiftCSV\uC640 \uAC19\uC740 \uD0C0\uC0AC \uB77C\
  \uC774\uBE0C\uB7EC\uB9AC\uB97C \uD65C\uC6A9\uD558\uC5EC \uB354 \uAC04\uD3B8\uD55C\
  \ \uBC29\uC2DD\uC73C\uB85C CSV \uB370\uC774\uD130\uB97C \uCC98\uB9AC\uD560 \uC218\
  \ \uC788\uC2B5\uB2C8\uB2E4."
title: "CSV\uC640 \uD568\uAED8 \uC791\uC5C5\uD558\uAE30"
weight: 37
---

## 방법:
Swift에서는 CSV 파일을 직접 파싱하기 위한 네이티브 지원이 없지만, `String` 메소드를 사용하여 내용을 분할하거나 SwiftCSV와 같은 타사 라이브러리를 활용하여 더 간편한 방식으로 CSV 데이터를 처리할 수 있습니다. 다음은 두 가지 방법입니다:

### 외부 라이브러리 없는 수동 파싱
```swift
// 간단한 CSV 문자열을 고려해보세요
let csvString = """
name,age,city
John Doe,29,New York
Jane Smith,34,Los Angeles
"""

// CSV 문자열을 줄로 분할
let rows = csvString.components(separatedBy: "\n")

// 첫 번째 줄에서 키 추출
let keys = rows.first?.components(separatedBy: ",")

// 두 번째 줄부터 시작하여 줄들을 반복 처리
var result: [[String: String]] = []
for row in rows.dropFirst() {
    let values = row.components(separatedBy: ",")
    let dict = Dictionary(uniqueKeysWithValues: zip(keys!, values))
    result.append(dict)
}

// 샘플 출력
print(result)
// 출력: [{"city": "New York", "age": "29", "name": "John Doe"}, {"city": "Los Angeles", "age": "34", "name": "Jane Smith"}]
```
이 접근법은 직관적이지만 특히 값 내의 콤마, 필드 내의 줄바꿈 등과 같은 특수한 경우를 포함하는 CSV 파일에는 견고성이 부족합니다.

### SwiftCSV 라이브러리 사용
먼저, `Package.swift` 종속성에 SwiftCSV를 포함하여 프로젝트에 추가하십시오:
```swift
.package(url: "https://github.com/swiftcsv/SwiftCSV.git", from: "0.5.6")
```
그런 다음 다음과 같이 가져오고 사용하세요:
```swift
import SwiftCSV

// `csvString`이 위와 같이 정의되었다고 가정

// CSV 객체 생성
if let csv = try? CSV(string: csvString) {
    // 사전으로 행 접근
    let rows = csv.namedRows
    
    // 샘플 출력
    print(rows)
    // 출력: [{"city": "New York", "age": "29", "name": "John Doe"}, {"city": "Los Angeles", "age": "34", "name": "Jane Smith"}]
}
```
SwiftCSV는 캡슐화된 콤마, 필드 내의 줄바꿈, 문자 인코딩과 같은 뉘앙스를 자동으로 처리하여 파싱을 단순화합니다. 그러나 외부 데이터 소스를 다룰 때 발생할 수 있는 가능한 오류, 특히 실세계 애플리케이션에서 오류 처리를 잊지 마십시오.
