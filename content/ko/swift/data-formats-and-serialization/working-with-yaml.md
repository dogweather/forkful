---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:03.134813-07:00
description: "YAML\uC740 \"YAML Ain't Markup Language\"\uC758 \uC57D\uC790\uC774\uBA70\
  , \uBAA8\uB4E0 \uD504\uB85C\uADF8\uB798\uBC0D \uC5B8\uC5B4\uB97C \uC704\uD55C \uC778\
  \uAC04 \uCE5C\uD654\uC801\uC778 \uB370\uC774\uD130 \uC9C1\uB82C\uD654 \uD45C\uC900\
  \uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uAD6C\uC131 \uD30C\
  \uC77C, \uD504\uB85C\uC138\uC2A4 \uAC04 \uBA54\uC2DC\uC9D5, \uB370\uC774\uD130 \uC800\
  \uC7A5\uC744 \uC704\uD574 YAML\uC744 \uC0AC\uC6A9\uD558\uB294\uB370, \uADF8 \uC774\
  \uC720\uB294 XML\uC774\uB098 JSON \uAC19\uC740 \uB2E4\uB978\u2026"
lastmod: '2024-03-13T22:44:55.760622-06:00'
model: gpt-4-0125-preview
summary: "YAML\uC740 \"YAML Ain't Markup Language\"\uC758 \uC57D\uC790\uC774\uBA70\
  , \uBAA8\uB4E0 \uD504\uB85C\uADF8\uB798\uBC0D \uC5B8\uC5B4\uB97C \uC704\uD55C \uC778\
  \uAC04 \uCE5C\uD654\uC801\uC778 \uB370\uC774\uD130 \uC9C1\uB82C\uD654 \uD45C\uC900\
  \uC785\uB2C8\uB2E4."
title: "YAML\uB85C \uC791\uC5C5\uD558\uAE30"
weight: 41
---

## 무엇 & 왜?
YAML은 "YAML Ain't Markup Language"의 약자이며, 모든 프로그래밍 언어를 위한 인간 친화적인 데이터 직렬화 표준입니다. 프로그래머들은 구성 파일, 프로세스 간 메시징, 데이터 저장을 위해 YAML을 사용하는데, 그 이유는 XML이나 JSON 같은 다른 데이터 포맷들에 비해 읽기가 훨씬 쉽고 일반 영어에 더 가깝기 때문에 이해하고 쓰기가 더 간단하기 때문입니다.

## 방법:
Swift는 YAML 파싱 및 직렬화에 대한 내장 지원이 없어, 서드파티 라이브러리를 사용할 필요가 있습니다. 인기 있는 선택은 Swift에서 YAML을 처리하기 위한 라이브러리인 `Yams`입니다.

먼저 프로젝트에 `Yams`를 추가해야 합니다. Swift Package Manager를 사용한다면, `Package.swift` 파일에 의존성으로 추가할 수 있습니다:

```swift
dependencies: [
    .package(url: "https://github.com/jpsim/Yams.git", from: "4.0.0")
]
```

### Swift로 YAML 파싱하기
간단한 앱을 위한 다음과 같은 YAML 구성을 가정해보겠습니다:

```yaml
name: MyApp
version: 1.0
environment: development
features:
  - login
  - notifications
```

`Yams`를 사용하여 Swift에서 이 YAML 문자열을 파싱하는 방법은 다음과 같습니다:

```swift
import Yams

let yamlString = """
name: MyApp
version: 1.0
environment: development
features:
  - login
  - notifications
"""

do {
    if let data = try Yams.load(yaml: yamlString) as? [String: Any] {
        print(data)
        // 파싱된 데이터에 접근하는 예시
        if let name = data["name"] as? String {
            print("앱 이름: \(name)")
        }
    }
} catch {
    print("YAML 파싱 오류: \(error)")
}
```

샘플 출력:

```
["name": MyApp, "version": 1.0, "environment": "development", "features": ["login", "notifications"]]
앱 이름: MyApp
```

### Swift 객체를 YAML로 직렬화하기
`Yams`를 사용하여 Swift 객체를 YAML 문자열로 다시 변환하는 것도 간단합니다. 직렬화할 동일한 데이터 구조를 가정합니다:

```swift
let appInfo = [
    "name": "MyApp",
    "version": 1.0,
    "environment": "development",
    "features": ["login", "notifications"]
] as [String : Any]

do {
    let yamlString = try Yams.dump(object: appInfo)
    print(yamlString)
} catch {
    print("YAML로 직렬화하는 중 오류: \(error)")
}
```

이렇게 하면 YAML 형식의 문자열이 생성됩니다:

```yaml
environment: development
features:
  - login
  - notifications
name: MyApp
version: 1.0
```

이 예시들은 Swift 애플리케이션에서 YAML을 작업하는 기본적인 작업을 보여줍니다. YAML이 인간의 읽기 용이성과 사용의 용이성에서 뛰어나지만, 데이터 직렬화 포맷을 선택할 때 특히 성능과 복잡성과 관련하여 애플리케이션의 특정 요구 사항을 항상 고려해야 합니다.
