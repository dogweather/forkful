---
date: 2024-01-26 00:59:13.061250-07:00
description: "\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694: Swift\uB294 `do`, `try`, `catch`\
  \ \uBE14\uB85D\uC744 \uC0AC\uC6A9\uD574 \uC624\uB958 \uCC98\uB9AC\uB97C \uD569\uB2C8\
  \uB2E4. \uD55C\uBC88 \uC0B4\uD3B4\uBCF4\uACA0\uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.742104-06:00'
model: gpt-4-1106-preview
summary: "Swift\uB294 `do`, `try`, `catch` \uBE14\uB85D\uC744 \uC0AC\uC6A9\uD574 \uC624\
  \uB958 \uCC98\uB9AC\uB97C \uD569\uB2C8\uB2E4."
title: "\uC5D0\uB7EC \uCC98\uB9AC\uD558\uAE30"
weight: 16
---

## 어떻게 하나요:
Swift는 `do`, `try`, `catch` 블록을 사용해 오류 처리를 합니다. 한번 살펴보겠습니다:

```Swift
enum FileError: Error {
    case fileDoesNotExist
    case noPermission
}

func readFile(atPath path: String) throws -> String {
    // 파일이 존재하는지, 그리고 읽기 권한이 있는지 검사하는 로직을 여기에 상상해보세요
    let fileExists = false
    let havePermission = true

    if !fileExists {
        throw FileError.fileDoesNotExist
    }

    if !havePermission {
        throw FileError.noPermission
    }

    return "파일 내용은 여기에 있습니다"
}

do {
    let fileContent = try readFile(atPath: "/path/to/file")
    print(fileContent)
} catch FileError.fileDoesNotExist {
    print("이런! 파일을 찾을 수 없습니다.")
} catch FileError.noPermission {
    print("아! 파일을 읽을 권한이 없습니다.")
} catch {
    print("알 수 없는 오류가 발생했습니다.")
}

```

샘플 출력:

```
이런! 파일을 찾을 수 없습니다.
```

## 깊게 들여다보기
오류 처리는 언제나 이처럼 매끈한 것은 아니었습니다. Objective-C에서는 NSError 객체에 대한 포인터를 다루며, 이는 다소 불편하게 느껴졌습니다. 이제 우리는 Swift 열거형(enum)과 `Error` 프로토콜로 더 우아한 시스템을 갖게 되었습니다.

Swift의 `throw`를 통해 무언가 잘못되었다고 알릴 수 있습니다. `do` 블록은 오류 인식 영역으로 작동하고, `try` 접두사는 위험한 작업을 호출하며, `catch`는 문제가 생겼을 때 처리합니다.

옵셔널은 실제 "오류" 상태까지는 아니지만 여전히 "결과가 없을" 수 있는 상황에 대한 대안입니다. 그것은 슈뢰딩거(Schrödinger)의 변수와 약간 비슷합니다 - 값이 있거나 없거나 둘 중 하나입니다.

진정한 깊이를 원한다면 정규 반환 패턴과 오류 패턴 사이의 세련된 하이브리드인 `Result` 타입을 살펴보세요.

## 또한 참조
- 공식 스위프트 오류 처리 가이드: [Apple 문서](https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html)
- 스위프트 오류 처리 모범 사례: [RayWenderlich.com](https://www.raywenderlich.com/1851-beginning-swift-error-handling)
- Swift에서의 고급 오류 처리: [Medium 글](https://medium.com/better-programming/advanced-error-handling-in-swift-4f6bdf6b01d8)
