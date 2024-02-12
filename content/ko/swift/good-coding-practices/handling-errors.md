---
title:                "에러 처리하기"
aliases:
- /ko/swift/handling-errors.md
date:                  2024-01-26T00:59:13.061250-07:00
model:                 gpt-4-1106-preview
simple_title:         "에러 처리하기"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/handling-errors.md"
---

{{< edit_this_page >}}

## 무엇과 왜?
Swift에서의 오류 처리란 코드 실행 중 발생하는 문제에 대비하고 대응하는 것을 말합니다. 이를 통해 혼돈을 제어하여 앱이 충돌하는 것을 방지하고 사용자에게 원활한 경험을 제공합니다.

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
