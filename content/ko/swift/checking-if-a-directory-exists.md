---
title:    "Swift: 디렉토리가 존재하는지 확인하는 방법"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## 왜?

컴퓨터 프로그래밍을 하다보면, 우리는 종종 파일이나 폴더가 존재하는지 여부를 확인해야 할 때가 있습니다. 예를 들어, 우리가 작성한 앱이 사용자의 디바이스에서 특정 폴더에 접근해야 할 때가 있습니다. 따라서 디렉토리가 존재하는지 확인하는 것은 중요한 일이 될 수 있습니다.

## 어떻게 할까요?

우선, 디렉토리가 현재 위치한 경로를 포함하는 `URL`을 만들어 줍니다. 이 `URL`은 `FileManager`의 `fileExists(atPath:)` 메서드를 호출하여 해당 경로의 파일이나 디렉토리가 실제로 존재하는지 확인할 수 있습니다.

```Swift
let fileManager = FileManager.default

let documentsURL = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!

let path = documentsURL.appendingPathComponent("example_folder")

if fileManager.fileExists(atPath: path.path) {
    print("\(path)는 이미 존재합니다.")
} else {
    print("\(path)는 존재하지 않습니다.")
}
```

위의 코드를 실행하면, `documents` 폴더에 `example_folder`라는 빈 폴더를 만들고, 그 폴더가 실제로 존재하는지 확인할 것입니다. 만약 폴더가 아직 생성되지 않았다면 `example_folder는 존재하지 않습니다.` 라는 메세지를 출력할 것입니다.

## 더 깊게 들어가보기

이렇게 간단하게 디렉토리가 존재하는지 여부를 체크할 수 있지만, 더 깊게 들어가보면 더 많은 가능성이 있습니다. 예를 들어, `FileManager`의 `fileExists(atPath:)` 메서드는 실제 파일이 존재하는지 확인한다는 것은 알지만, 해당 파일이 폴더인지 체크하지는 않습니다. 이 때는 `isDirectory` 프로퍼티를 사용하여 폴더인지 확인할 수 있습니다. 또한, 해당 `URL`이 실제로 파일인지 디렉토리인지 확인하는 방법도 있습니다.

그 외에도 `FileManager`는 다양한 메서드를 가지고 있으므로, 필요에 따라 찾아보고 사용할 수 있습니다. 더 많은 정보는 [Apple 개발자 문서](https://developer.apple.com/documentation/foundation/filemanager)를 참고해주세요.

## See Also

- [Apple 개발자 문서: FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Swift Tutorials: Working with Directories in Swift](https://www.swiftdevcenter.com/working-directories-swift/)