---
title:                "디렉터리가 존재하는지 확인하기"
html_title:           "Swift: 디렉터리가 존재하는지 확인하기"
simple_title:         "디렉터리가 존재하는지 확인하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 무엇이고 왜?

디렉토리의 존재 여부를 확인하는 것은 프로그래머들이 많이 사용하는 기능입니다. 이를 통해 우리는 파일의 유무를 체크하거나, 디렉토리에 파일을 생성하기 전에 확인할 수 있습니다.

## 하기 방법:
```Swift
if FileManager.default.fileExists(atPath: "myDir") {
    print("디렉토리가 존재합니다")
} else {
    print("디렉토리가 존재하지 않습니다")
}
```

위의 예제를 통해 우리는 `FileManager` 클래스의 `fileExists` 메소드를 사용하여 디렉토리의 존재 여부를 확인할 수 있습니다. `if` 문을 사용하여 디렉토리가 존재하는 경우와 존재하지 않는 경우에 대해 각각 다른 출력을 하도록 설정할 수 있습니다.

## 깊이 알아보기:
디렉토리의 존재 여부를 확인하는 것은 파일 시스템의 개념 중 하나입니다. 파일 시스템은 파일과 폴더를 모아서 관리하는 방식입니다. 이를 통해 우리는 우리가 필요로 하는 파일들을 쉽게 찾을 수 있고, 다양한 작업을 할 수 있습니다. 디렉토리의 존재 여부를 확인하기 위해 다른 방법으로는 `NSFileManager` 클래스의 `fileExistsAtPath` 메소드를 사용할 수도 있습니다. 이 클래스는 이전 버전의 스위프트에서 사용되었으며, 현재 버전에서는 `FileManager`를 사용하는 것이 권장됩니다.

## 더 알아보기:
- [File System](https://en.wikipedia.org/wiki/File_system)
- [NSFileManager Class Reference](https://developer.apple.com/documentation/foundation/nsfilemanager)
- [FileManager Class Reference](https://developer.apple.com/documentation/foundation/filemanager)