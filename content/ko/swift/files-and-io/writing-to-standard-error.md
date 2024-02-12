---
title:                "표준 에러에 쓰기"
aliases:
- /ko/swift/writing-to-standard-error.md
date:                  2024-02-03T19:34:58.507486-07:00
model:                 gpt-4-0125-preview
simple_title:         "표준 에러에 쓰기"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇이며 왜인가?

표준 오류(stderr) 쓰기는 프로그램의 오류 메시지나 진단 출력을 표준 출력(stdout)과 구별되는 별도의 스트림으로 전달하는 것에 관한 것입니다. 이는 표준 출력을 혼잡하게 만들지 않고 디버깅과 오류 로깅을 용이하게 함으로써 개발자와 사용자 모두가 프로그램의 상태와 문제를 이해하는 데 있어 중요합니다.

## 방법:

Swift에서 표준 오류에 쓰기는 직접적으로 stderr 접근을 위한 `FileHandle` 클래스를 사용하여 수행될 수 있습니다. 간단한 예제는 다음과 같습니다:

```swift
import Foundation

// 메시지 정의
let errorMessage = "An error occurred.\n"

// 메시지를 데이터로 변환
if let data = errorMessage.data(using: .utf8) {
    // stderr에 에러 메시지 쓰기
    FileHandle.standardError.write(data)
}
```

stderr로의 출력(일반적으로 콘솔이나 터미널에서 볼 수 있음):
```
An error occurred.
```

보다 복잡한 로깅이나 외부 라이브러리와 함께 작업할 때는 **SwiftLog**과 같은 서드파티 라이브러리 사용을 고려할 수 있습니다. **SwiftLog**은 기본적으로 직접 stderr에 쓰지 않지만, 커스텀 로깅 백엔드를 구현하여 이를 달성할 수 있습니다. stderr에 쓰기 위한 사용자 정의 로그 핸들러를 정의하는 간소화된 예는 다음과 같습니다:

먼저 `Package.swift`에서 프로젝트 의존성에 **SwiftLog**을 추가하세요:
```swift
// swift-tools-version:5.3

import PackageDescription

let package = Package(
    name: "YourPackageName",
    dependencies: [
        .package(url: "https://github.com/apple/swift-log.git", from: "1.0.0"),
    ],
    targets: [
        .target(
            name: "YourTargetName",
            dependencies: [
                .product(name: "Logging", package: "swift-log"),
            ]),
    ]
)
```

그런 다음 stderr에 쓰는 사용자 정의 로그 핸들러를 구현하세요:

```swift
import Logging
import Foundation

struct StderrLogHandler: LogHandler {
    let label: String
    
    var logLevel: Logger.Level = .info
    
    func log(level: Logger.Level, message: Logger.Message, metadata: Logger.Metadata?, source: String, file: String, function: String, line: UInt) {
        let output = "\(message)\n"
        if let data = output.data(using: .utf8) {
            FileHandle.standardError.write(data)
        }
    }
    
    subscript(metadataKey metadataKey: String) -> Logger.Metadata.Value? {
        get { return nil }
        set(newValue) { }
    }
    
    var metadata: Logger.Metadata {
        get { return [:] }
        set(newMetadata) { }
    }
}

// 사용법
LoggingSystem.bootstrap(StderrLogHandler.init)
let logger = Logger(label: "com.example.yourapp")

logger.error("This is an error message")
```

stderr로의 출력:
```
This is an error message
```

이 사용자 정의 핸들러를 사용하면 SwiftLog 오류 메시지를 직접 표준 오류로 라우팅할 수 있어, 애플리케이션이 생성할 수 있는 다른 로그 메시지와 원활하게 통합됩니다.
