---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:40.144216-07:00
description: "Swift\uB85C \uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC744 \uC791\uC131\uD558\
  \uBA74 \uBB38\uC790\uC5F4 \uB370\uC774\uD130\uB97C \uD30C\uC77C \uC2DC\uC2A4\uD15C\
  \uC5D0 \uC9C0\uC18D\uC801\uC73C\uB85C \uC800\uC7A5\uD560 \uC218 \uC788\uC73C\uBA70\
  , \uC774\uB294 \uAD6C\uC131 \uC124\uC815, \uC0AC\uC6A9\uC790 \uB370\uC774\uD130\
  \ \uB610\uB294 \uB85C\uADF8\uB97C \uC800\uC7A5\uD558\uB294 \uC791\uC5C5\uC5D0 \uD544\
  \uC218\uC801\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC885\
  \uC885 \uC571 \uC2E4\uD589 \uC0AC\uC774\uC758 \uB370\uC774\uD130\uB97C \uC720\uC9C0\
  \uD558\uAC70\uB098, \uC560\uD50C\uB9AC\uCF00\uC774\uC158\uC758 \uB2E4\uB978 \uBD80\
  \uBD84 \uAC04\uC5D0 \uB370\uC774\uD130\uB97C\u2026"
lastmod: '2024-02-25T18:49:52.745588-07:00'
model: gpt-4-0125-preview
summary: "Swift\uB85C \uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC744 \uC791\uC131\uD558\uBA74\
  \ \uBB38\uC790\uC5F4 \uB370\uC774\uD130\uB97C \uD30C\uC77C \uC2DC\uC2A4\uD15C\uC5D0\
  \ \uC9C0\uC18D\uC801\uC73C\uB85C \uC800\uC7A5\uD560 \uC218 \uC788\uC73C\uBA70, \uC774\
  \uB294 \uAD6C\uC131 \uC124\uC815, \uC0AC\uC6A9\uC790 \uB370\uC774\uD130 \uB610\uB294\
  \ \uB85C\uADF8\uB97C \uC800\uC7A5\uD558\uB294 \uC791\uC5C5\uC5D0 \uD544\uC218\uC801\
  \uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC885\uC885 \uC571\
  \ \uC2E4\uD589 \uC0AC\uC774\uC758 \uB370\uC774\uD130\uB97C \uC720\uC9C0\uD558\uAC70\
  \uB098, \uC560\uD50C\uB9AC\uCF00\uC774\uC158\uC758 \uB2E4\uB978 \uBD80\uBD84 \uAC04\
  \uC5D0 \uB370\uC774\uD130\uB97C\u2026"
title: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC4F0\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?

Swift로 텍스트 파일을 작성하면 문자열 데이터를 파일 시스템에 지속적으로 저장할 수 있으며, 이는 구성 설정, 사용자 데이터 또는 로그를 저장하는 작업에 필수적입니다. 프로그래머들은 종종 앱 실행 사이의 데이터를 유지하거나, 애플리케이션의 다른 부분 간에 데이터를 공유하거나, 다른 프로그램에서 사용할 데이터를 내보내기 위해 이 작업을 합니다.

## 방법:

### Swift 표준 라이브러리 사용하기

Swift의 표준 라이브러리에는 텍스트 파일을 작성하는 데 필요한 모든 도구가 포함되어 있습니다. 기본적인 접근 방법은 다음과 같습니다:

```swift
import Foundation

let content = "Hello, Wired readers! Learning Swift is fun."
let filePath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] as String
let fileName = "\(filePath)/example.txt"

do {
    try content.write(toFile: fileName, atomically: false, encoding: String.Encoding.utf8)
    print("File written successfully")
} catch let error as NSError {
    print("Failed writing to URL: \(fileName), Error: " + error.localizedDescription)
}
```

이 코드 스니펫은 문서 디렉토리에 `example.txt`라는 이름의 파일에 문자열을 작성합니다. Swift의 do-try-catch 오류 처리를 사용하여 잠재적 오류를 처리합니다.

### 파일 속성 제어나 파일 존재 여부 확인을 위해 FileManager 사용하기

파일 속성에 대한 더 많은 제어력을 원하거나 파일이 이미 존재하는지 확인하려면, `FileManager`를 사용할 수 있습니다:

```swift
import Foundation

let fileManager = FileManager.default
let directories = fileManager.urls(for: .documentDirectory, in: .userDomainMask)
if let documentDirectory = directories.first {
    let fileURL = documentDirectory.appendingPathComponent("example.txt")
    let content = "Exploring Swift for file management is enlightening."

    if fileManager.fileExists(atPath: fileURL.path) {
        print("File already exists")
    } else {
        do {
            try content.write(to: fileURL, atomically: true, encoding: .utf8)
            print("File created and written successfully")
        } catch {
            print("Error writing file: \(error)")
        }
    }
}
```

### 타사 라이브러리 사용하기

Swift에서 파일 시스템 작업을 위한 인기 있는 타사 라이브러리 중 하나는 John Sundell의 `Files`입니다:

먼저, Swift 패키지 매니저를 통해 프로젝트에 Files를 추가하세요.

```swift
// swift-tools-version:5.3
import PackageDescription

let package = Package(
    name: "YourPackageName",
    dependencies: [
        .package(url: "https://github.com/JohnSundell/Files", from: "4.0.0"),
    ],
    targets: [
        .target(
            name: "YourTargetName",
            dependencies: ["Files"]),
    ]
)
```

그런 다음, 파일에 작성하는 데 사용하세요:

```swift
import Files

do {
    let file = try File(path: "/path/to/your/directory/example.txt")
    try file.write(string: "Swift and Files library make a powerful combination.")
    print("File written successfully using Files library.")
} catch {
    print("An error occurred: \(error)")
}
```

`Files` 라이브러리를 사용하면 파일 처리가 더 간단해져 애플리케이션의 비즈니스 로직에 집중할 수 있게 해줍니다. 파일 관리의 복잡성보다는요.
