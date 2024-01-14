---
title:                "Swift: 디렉토리의 존재 여부 확인하기"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜

디렉토리가 존재하는지 확인하는 것이 왜 중요할까요? 주어진 경로가 실제로 있는지 확인할 때, 앱 내에서 필요한 로직을 실행하기 전에 디렉토리가 존재하는지 확인하는 것은 중요합니다. 예를 들어, 파일을 쓰거나 읽을 때, 해당 파일이 있는 경로가 정확한지 확인하기 위해 디렉토리가 존재하는지 먼저 확인해야 합니다.

## 하는 방법

다음은 디렉토리가 존재하는지를 확인하는 기본적인 코드 예제입니다.

```Swift 
func checkDirectoryExists(atPath path: String) -> Bool {
    let fileManager = FileManager.default
    var isDirectory: ObjCBool = false
    let exists = fileManager.fileExists(atPath: path, isDirectory: &isDirectory)
    return exists && isDirectory.boolValue
}
```

위의 함수는 주어진 경로가 디렉토리이면 `true`를 반환하고 그렇지 않으면 `false`를 반환합니다. 이 함수는 `FileManager`의 `fileExists(atPath:isDirectory:)` 메서드를 사용합니다. 이 메서드는 주어진 경로에 파일이나 디렉토리가 존재하는지를 나타내는 `exists`를 반환하고 `isDirectory`에 해당 경로가 디렉토리인지를 `bool` 값으로 저장합니다.

이제 위의 함수를 사용해보겠습니다.

```Swift
let documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0]
let directoryExists = checkDirectoryExists(atPath: documentsPath)
print("Documents directory exists: \(directoryExists)")
```

위의 코드는 Documents 디렉토리가 앱 내에 존재하는지를 확인한 뒤, 그 결과를 출력합니다. 출력은 다음과 같이 나올 것입니다.

```
Documents directory exists: true
```

## 깊게 파고들기

디렉토리가 존재하는지 확인하는 것은 단순한 작업처럼 보이지만, 실제로는 여러 가지 예외 상황에 대응해야 합니다. 예를 들어, 앱 내에서 파일을 읽거나 쓸 때 해당 디렉토리에 대한 권한이 없는 경우, 디렉토리가 삭제되었을 경우 등을 고려해야 합니다. 이를 위해 디렉토리가 존재하지 않는 경우에 대한 오류 처리를 추가하는 것이 좋습니다.

또한, 디렉토리가 존재하는지 확인하는 것과 함께 해당 디렉토리에 접근하여 파일을 읽고 쓰는 방법도 중요합니다. 디렉토리가 있음을 확인하더라도 파일 이름에 대한 유효성 검사를 충분히 수행하지 않는다면, 다른 문제가 발생할 수 있습니다. 디렉토리 존재 여부만 확인하는 것이 아니라, 해당 디렉토리 내의 파일들에 대한 유효성 검사를 함께 해주는 것이 좋습니다.

이러한 깊은 이해를 바탕으로 디렉토리가 존재하는지 확인하는 방법을 더욱 효율적으로 구현할 수 있습니다.

## 다른 글

- [WWDC 2020: File Management in Swift](https://developer.apple.com/videos/play/wwdc2020/10629/)
- [Managing Files and Directories](https://developer.apple.com/documentation/foundation/file_management)