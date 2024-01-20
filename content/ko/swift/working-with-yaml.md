---
title:                "YAML 다루기"
html_title:           "Arduino: YAML 다루기"
simple_title:         "YAML 다루기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML은 설정 파일 등에 쓰이는 데이터 형식입니다. 간결하고 읽기 쉬워 프로그래머들이 설정, 직렬화 및 데이터 저장을 위해 사용합니다.

## How to:
Swift에서 YAML 다루려면 라이브러리 필요. 여기 `Yams` 사용 예시:

```Swift
import Yams

let yamlString = """
name: Yuna
age: 25
languages:
  - Korean
  - English
"""

// YAML 문자열 파싱
if let person = try? Yams.load(yaml: yamlString) as? [String: Any] {
    print(person)
}

// 출력 결과
// ["name": "Yuna", "age": 25, "languages": ["Korean", "English"]]
```

## Deep Dive:
YAML은 "YAML Ain't Markup Language" (원래는 "Yet Another Markup Language") 약자. JSON과 XML 대안이며, 사람이 읽을 수 있는 데이터 직렬화 표준입니다. Swift에선 `Yams`, `SwiftYAML` 등의 라이브러리로 YAML 처리 가능. `Yams`는 LibYAML기반, 속도와 안정성 강점 있습니다.

## See Also:
- YAML 표준 스펙: https://yaml.org/spec/1.2/spec.html
- Yams 라이브러리 GitHub 페이지: https://github.com/jpsim/Yams
- Swift 패키지 매니저 문서: https://swift.org/package-manager/