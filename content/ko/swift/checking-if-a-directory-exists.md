---
title:                "디렉토리 존재 여부 확인하기"
date:                  2024-01-20T14:58:33.187313-07:00
html_title:           "Fish Shell: 디렉토리 존재 여부 확인하기"
simple_title:         "디렉토리 존재 여부 확인하기"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
디렉토리 존재 확인은 파일 시스템에서 특정 경로에 디렉토리가 있는지 체크하는 거예요. 프로그래머들이 이걸 하면 에러 없이 파일 작업을 시작할 수 있어요.

## How to: (어떻게 하나요?)
```Swift
import Foundation

let fileManager = FileManager.default
let path = "/some/path/to/directory"

if fileManager.fileExists(atPath: path, isDirectory: nil) {
    print("Directory exists!")
} else {
    print("Directory does not exist.")
}
```

Sample Output:
```
Directory exists!
```
or
```
Directory does not exist.
```

## Deep Dive (심층 분석)
Swift에서 디렉토리 존재를 확인하는 것은 `FileManager` 클래스를 사용합니다. 이 클래스는 NeXTSTEP 시대부터 UNIX 파일 시스템과 상호 작용하는 일반적인 방법입니다. `fileExists(atPath:)`은 경로 존재 여부를 확인할 수 있지만, `isDirectory` 포인터를 사용하면 해당 경로가 실제로 디렉토리인지 파일인지를 구별할 수 있어요. 대안으로 `URL` 리소스 프로퍼티를 사용하거나 POSIX 함수를 사용할 수도 있지만, Swift 표준 라이브러리를 쓸 때는 `FileManager`가 가장 일반적이고 쉬운 방법입니다.

## See Also (더 알아보기)
- Swift Documentation on FileManager: [FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- Apple's File System Programming Guide: [File System Basics](https://developer.apple.com/library/archive/documentation/FileManagement/Conceptual/FileSystemProgrammingGuide/FileSystemOverview/FileSystemOverview.html)
