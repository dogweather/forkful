---
aliases:
- /ko/swift/working-with-csv/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:34.346087-07:00
description: "CSV(\uCF64\uB9C8\uB85C \uAD6C\uBD84\uB41C \uAC12) \uD30C\uC77C \uC791\
  \uC5C5\uC740 \uAC01 \uC904\uC774 \uD558\uB098\uC758 \uB808\uCF54\uB4DC\uB97C \uB098\
  \uD0C0\uB0B4\uACE0 \uAC01 \uB808\uCF54\uB4DC\uAC00 \uCF64\uB9C8\uB85C \uAD6C\uBD84\
  \uB41C \uD544\uB4DC\uB85C \uC774\uB8E8\uC5B4\uC9C4 \uD14D\uC2A4\uD2B8 \uD30C\uC77C\
  \uC5D0\uC11C \uAD6C\uC870\uD654\uB41C \uB370\uC774\uD130\uB97C \uD30C\uC2F1\uD558\
  \uACE0 \uC0DD\uC131\uD558\uB294 \uAC83\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4. \uD504\
  \uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC885\uC885 \uC774 \uD65C\uB3D9\uC5D0 \uCC38\
  \uC5EC\uD558\uB294\uB370, \uC774\uB294 \uB2E4\uC591\uD55C \uD50C\uB7AB\uD3FC\uACFC\
  \ \uD504\uB85C\uADF8\uB798\uBC0D \uC5B8\uC5B4\uC5D0\uC11C \uB110\uB9AC\u2026"
lastmod: 2024-02-18 23:09:06.783388
model: gpt-4-0125-preview
summary: "CSV(\uCF64\uB9C8\uB85C \uAD6C\uBD84\uB41C \uAC12) \uD30C\uC77C \uC791\uC5C5\
  \uC740 \uAC01 \uC904\uC774 \uD558\uB098\uC758 \uB808\uCF54\uB4DC\uB97C \uB098\uD0C0\
  \uB0B4\uACE0 \uAC01 \uB808\uCF54\uB4DC\uAC00 \uCF64\uB9C8\uB85C \uAD6C\uBD84\uB41C\
  \ \uD544\uB4DC\uB85C \uC774\uB8E8\uC5B4\uC9C4 \uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC5D0\
  \uC11C \uAD6C\uC870\uD654\uB41C \uB370\uC774\uD130\uB97C \uD30C\uC2F1\uD558\uACE0\
  \ \uC0DD\uC131\uD558\uB294 \uAC83\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4. \uD504\uB85C\
  \uADF8\uB798\uBA38\uB4E4\uC740 \uC885\uC885 \uC774 \uD65C\uB3D9\uC5D0 \uCC38\uC5EC\
  \uD558\uB294\uB370, \uC774\uB294 \uB2E4\uC591\uD55C \uD50C\uB7AB\uD3FC\uACFC \uD504\
  \uB85C\uADF8\uB798\uBC0D \uC5B8\uC5B4\uC5D0\uC11C \uB110\uB9AC\u2026"
title: "CSV\uC640 \uD568\uAED8 \uC791\uC5C5\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?

CSV(콤마로 구분된 값) 파일 작업은 각 줄이 하나의 레코드를 나타내고 각 레코드가 콤마로 구분된 필드로 이루어진 텍스트 파일에서 구조화된 데이터를 파싱하고 생성하는 것을 포함합니다. 프로그래머들은 종종 이 활동에 참여하는데, 이는 다양한 플랫폼과 프로그래밍 언어에서 널리 지원되는 형식을 사용하여 테이블 데이터를 쉽게 가져오고, 내보내고, 조작할 수 있기 때문입니다. 그 이유는 그것의 단순성과 사람이 읽을 수 있는 형식 때문입니다.

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
