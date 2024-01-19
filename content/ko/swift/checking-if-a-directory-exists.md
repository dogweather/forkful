---
title:                "디렉토리가 존재하는지 확인하기"
html_title:           "C#: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 필요할까? (_What & Why?_)
디렉토리 존재 확인은 파일 시스템에서 특정 경로의 디렉토리가 있는지를 검사하는 행위입니다. 이는 코드 실행 중에 예상치 못한 오류를 방지하고, 파일 또는 디렉토리 작업을 보다 안전하게 수행하기 위해 프로그래머들이 사용합니다.

## 방법 (_How to:_)
Swift에서는 `FileManager` 클래스를 사용하여 디렉토리 존재 여부를 확인할 수 있습니다. 아래 코드는 개념을 설명합니다:

```Swift
import Foundation

let fileManager = FileManager.default
let directoryPath = "/path/to/directory"

var isDir : ObjCBool = false
if fileManager.fileExists(atPath: directoryPath, isDirectory: &isDir) {
    if isDir.boolValue {
        print("\(directoryPath) does exist!")
    } else {
        print("\(directoryPath) does not exist!")
    }
}
```

위 코드에서는 `/path/to/directory`가 실제로 존재하는 디렉토리인지를 검사합니다.

## Deep Dive
디렉토리 존재 여부를 확인하는 것은 파일 시스템을 안전하게 조작하는데 중요한 과정입니다. 이를 체크하지 않으면 존재하지 않는 디렉토리에 접근하거나 데이터를 쓰려고 할 때 실행 시간 오류가 발생할 수 있습니다.

서로 다른 선택지로 Linux의 shell 스크립트에서는 `test -d` 또는 `[ -d ]` 구문을 사용하여 디렉토리의 존재 여부를 확인할 수 있습니다. 그러나 Swift는 타입 안정성을 제공하고 오류 처리가 더 쉽기 때문에 많은 iOS 개발자들이 선호합니다.

`fileExists(atPath:isDirectory:)` 메소드는 첫 번째 매개 변수로 경로를, 두 번째 매개 변수로 Bool 값을 참조로 받아 해당 경로가 디렉토리인지 아니면 파일인지를 판단하여 반환합니다.

## 참고자료 (_See Also_)
1. [Apple Developer Documentation - FileManager](https://developer.apple.com/documentation/foundation/filemanager)
2. [Swift Documentation](https://swift.org/documentation/)
3. [StackOverflow - How to check if a directory exists in Swift?](https://stackoverflow.com/questions/24070450/how-to-check-if-a-directory-exists-in-swift)