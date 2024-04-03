---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:49.788391-07:00
description: "\uBC29\uBC95: Swift\uC758 Foundation \uD504\uB808\uC784\uC6CC\uD06C\uB294\
  \ \uD30C\uC77C \uC2DC\uC2A4\uD15C\uC744 \uAD00\uB9AC\uD560 \uC218 \uC788\uB294 \uBA54\
  \uC18C\uB4DC\uB97C \uAC00\uC9C4 `FileManager` \uD074\uB798\uC2A4\uB97C \uC81C\uACF5\
  \uD569\uB2C8\uB2E4. `FileManager`\uB97C \uC0AC\uC6A9\uD558\uC5EC \uB514\uB809\uD1A0\
  \uB9AC\uAC00 \uC874\uC7AC\uD558\uB294\uC9C0 \uD655\uC778\uD560 \uC218 \uC788\uC2B5\
  \uB2C8\uB2E4. \uC774\uB97C \uC218\uD589\uD558\uB294 \uBC29\uBC95\uC5D0 \uB300\uD55C\
  \ \uCF54\uB4DC \uC2A4\uB2C8\uD3AB\uC740 \uB2E4\uC74C\uACFC\u2026"
lastmod: '2024-03-13T22:44:55.751901-06:00'
model: gpt-4-0125-preview
summary: "Swift\uC758 Foundation \uD504\uB808\uC784\uC6CC\uD06C\uB294 \uD30C\uC77C\
  \ \uC2DC\uC2A4\uD15C\uC744 \uAD00\uB9AC\uD560 \uC218 \uC788\uB294 \uBA54\uC18C\uB4DC\
  \uB97C \uAC00\uC9C4 `FileManager` \uD074\uB798\uC2A4\uB97C \uC81C\uACF5\uD569\uB2C8\
  \uB2E4."
title: "\uB514\uB809\uD1A0\uB9AC\uAC00 \uC874\uC7AC\uD558\uB294\uC9C0 \uD655\uC778\
  \uD558\uAE30"
weight: 20
---

## 방법:
Swift의 Foundation 프레임워크는 파일 시스템을 관리할 수 있는 메소드를 가진 `FileManager` 클래스를 제공합니다. `FileManager`를 사용하여 디렉토리가 존재하는지 확인할 수 있습니다. 이를 수행하는 방법에 대한 코드 스니펫은 다음과 같습니다:

```swift
import Foundation

let fileManager = FileManager.default
let path = "/path/to/your/directory"

if fileManager.fileExists(atPath: path, isDirectory: nil) {
    print("Directory exists")
} else {
    print("Directory does not exist")
}
```

하지만, 이 방법은 파일과 디렉토리 모두에 대해 확인합니다. 디렉토리가 존재하는지 구체적으로 확인하고 싶다면, `isDirectory`에 부울 값의 포인터를 전달해야 합니다:

```swift
import Foundation

let fileManager = FileManager.default
let path = "/path/to/your/directory"
var isDirectory: ObjCBool = false

if fileManager.fileExists(atPath: path, isDirectory: &isDirectory), isDirectory.boolValue {
    print("Directory exists")
} else {
    print("Directory does not exist")
}
```

### 타사 라이브러리 사용하기
현재로서는, Swift에서 디렉토리의 존재 유무를 확인하기 위해 타사 라이브러리를 필요로 하는 경우는 드뭅니다. 이는 `FileManager` 클래스의 강력함 때문입니다. 그러나, 더 복잡한 파일 조작 및 확인이 필요한 경우, John Sundell이 제공하는 **Files**와 같은 라이브러리는 더 Swift 친화적인 API를 제공합니다.

이를 사용하는 방법은 다음과 같습니다:

먼저, Swift 패키지 관리자를 통해 프로젝트에 Files를 추가합니다.

그런 다음, 다음과 같이 디렉토리의 존재 유무를 확인할 수 있습니다:

```swift
import Files

do {
    _ = try Folder(path: "/path/to/your/directory")
    print("Directory exists")
} catch {
    print("Directory does not exist")
}
```

참고: 타사 라이브러리는 변경될 수 있으므로, 사용법과 모범 사례에 대해서는 항상 최신 문서를 참조하세요.
