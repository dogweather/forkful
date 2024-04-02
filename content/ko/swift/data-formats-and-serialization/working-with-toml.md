---
date: 2024-01-26 04:26:51.074920-07:00
description: "TOML (Tom's Obvious, Minimal Language)\uC740 \uBA85\uD655\uD55C \uC758\
  \uBBF8\uB860\uC73C\uB85C \uC778\uD574 \uC77D\uAE30 \uC26C\uC6B4 \uB370\uC774\uD130\
  \ \uC9C1\uB82C\uD654 \uD3EC\uB9F7\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uC0AC\uB78C\uC774 \uC77D\uAE30 \uC27D\uACE0 \uAE30\uACC4\uAC00 \uC27D\
  \uAC8C \uD30C\uC2F1\uD560 \uC218 \uC788\uB294 \uAD6C\uC131 \uD30C\uC77C\uC5D0 TOML\uC744\
  \ \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.765232-06:00'
model: gpt-4-0125-preview
summary: "TOML (Tom's Obvious, Minimal Language)\uC740 \uBA85\uD655\uD55C \uC758\uBBF8\
  \uB860\uC73C\uB85C \uC778\uD574 \uC77D\uAE30 \uC26C\uC6B4 \uB370\uC774\uD130 \uC9C1\
  \uB82C\uD654 \uD3EC\uB9F7\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\
  \uC740 \uC0AC\uB78C\uC774 \uC77D\uAE30 \uC27D\uACE0 \uAE30\uACC4\uAC00 \uC27D\uAC8C\
  \ \uD30C\uC2F1\uD560 \uC218 \uC788\uB294 \uAD6C\uC131 \uD30C\uC77C\uC5D0 TOML\uC744\
  \ \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uD504\uB85C\uADF8\uB798\uBA38\uB97C \uC704\uD55C TOML \uB2E4\uB8E8\uAE30"
weight: 39
---

## 무엇 & 왜?
TOML (Tom's Obvious, Minimal Language)은 명확한 의미론으로 인해 읽기 쉬운 데이터 직렬화 포맷입니다. 프로그래머들은 사람이 읽기 쉽고 기계가 쉽게 파싱할 수 있는 구성 파일에 TOML을 사용합니다.

## 방법:
시작하려면 TOML 파서가 필요합니다. Swift에는 내장된 파서가 없으므로 `TOMLDecoder`를 사용합시다. Swift 패키지 관리자를 통해 설치한 다음 TOML을 쉽게 직렬화하고 역직렬화합니다.

```Swift
import TOMLDecoder

let tomlString = """
title = "TOML Example"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
"""

struct Config: Codable {
    let title: String
    let owner: Owner
}

struct Owner: Codable {
    let name: String
    let dob: Date
}

let decoder = TOMLDecoder()
if let configData = tomlString.data(using: .utf8) {
    do {
        let config = try decoder.decode(Config.self, from: configData)
        print("제목: \(config.title), 소유자: \(config.owner.name), 생년월일: \(config.owner.dob)")
    } catch {
        print("TOML 파싱 중 오류가 발생했습니다: \(error)")
    }
}
```

이 코드의 출력:
```
제목: TOML Example, 소유자: Tom Preston-Werner, 생년월일: 1979-05-27 07:32:00 +0000
```

## 심도 있는 탐구
TOML은 GitHub의 공동 창립자인 Tom Preston-Werner에 의해 설계되었으며, JSON이나 YAML과 같은 포맷보다 인간 친화적인 대안으로 목표를 두고 있습니다. 그것은 명확함을 추구하여 인간이나 기계에 의한 오해의 가능성을 줄입니다. 대안으로, YAML과 JSON이 일반적으로 언급되며, YAML은 인간의 가독성 쪽으로, JSON은 더 간단한 기계 친화적 옵션으로 기울어져 있습니다. Swift에서 TOML을 사용할 때, 우리는 네이티브 파서를 가지고 있지 않습니다. 하지만, `TOMLDecoder`와 같은 제3자 라이브러리는 TOML 문자열과 Swift 타입 간의 쉬운 변환을 가능하게 하며, 특히 Swift 4에서 도입된 `Codable` 프로토콜을 통해 직렬화를 간소화하였습니다.

## 참고
- TOML 표준: https://toml.io
- `TOMLDecoder`의 GitHub: https://github.com/dduan/TOMLDecoder
- `Codable`에 대한 Swift 문서: https://developer.apple.com/documentation/swift/codable
- 데이터 직렬화 포맷 비교: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
