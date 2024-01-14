---
title:    "Swift: 디렉터리가 존재하는지 확인하는 방법"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

Korean translation below:

# 왜 디렉토리 존재 여부를 확인해야 할까요?

디렉토리 존재 여부를 확인하는 것은 파일 또는 폴더가 존재하는지에 대한 정보를 얻기 위해서입니다. 이를 통해 코드가 존재하지 않는 디렉토리에 접근하거나 이미 존재하는 디렉토리를 다시 만드는 문제를 방지할 수 있습니다.

## 어떻게 확인할 수 있나요?

```Swift
let fileManager = FileManager.default
// 디렉토리 경로 설정
let directoryPath = "./Documents/MyFolder"
// 존재 여부 확인
var isDirectory: ObjCBool = false
_ = fileManager.fileExists(atPath: directoryPath, isDirectory: &isDirectory)
// 출력 결과
print(isDirectory.boolValue) // 해당 디렉토리가 존재하면 true, 존재하지 않으면 false
```

## 디렉토리 존재 여부에 대한 더 깊은 이해

디렉토리 존재 여부를 확인하는 데에는 `fileExists` 메소드와 `isDirectory` 매개변수를 사용하는 것이 일반적입니다. 이 때, `fileExists` 메소드는 해당 경로에 파일 또는 폴더가 있는지 확인하며, `isDirectory` 매개변수를 통해 파일인지 폴더인지 구분할 수 있습니다. 이를 통해 다른 적절한 처리를 할 수 있습니다.

# 관련 자료

- [FileManager documentation](https://developer.apple.com/documentation/foundation/filemanager)
- [Stack Overflow post on checking directory existence in Swift](https://stackoverflow.com/questions/24465466/how-to-check-if-a-file-exists-in-the-documents-directory-in-swift)
- [SwiftBites article on checking directory existence in Swift](https://www.swiftbites.net/posts/checking-if-a-file-directory-exists-in-swift/)