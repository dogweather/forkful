---
title:                "Swift: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜
컴퓨터 프로그래밍을 하다보면 종종 특정 디렉토리가 존재하는지 여부를 확인해야 할 때가 있습니다. 디렉토리가 존재하는지 확인하는 것은 프로그램의 안정성을 위해 매우 중요합니다. 따라서 이번 블로그 포스트에서는 Swift 프로그래밍을 할 때 디렉토리가 존재하는지 확인하는 방법에 대해 알아보겠습니다.

## 어떻게
디렉토리의 존재 여부를 확인하는 가장 간단한 방법은 FileManager 클래스의 `fileExists(atPath:)` 메서드를 사용하는 것입니다. 이 메서드는 디렉토리의 경로를 파라미터로 받아 해당 경로에 파일이나 디렉토리가 존재하는지 여부를 Bool 값으로 반환합니다.

```Swift
let fileManager = FileManager.default
let path = "/Users/example/Documents/New Folder" // 디렉토리 경로 지정
if fileManager.fileExists(atPath: path) {
  print("디렉토리가 존재합니다.")
} else {
  print("디렉토리가 존재하지 않습니다.")
}
```

위의 예시에서는 `path` 변수에 존재하는 디렉토리의 경로를 지정하고, `fileExists(atPath:)` 메서드를 사용하여 디렉토리가 존재하는지를 확인합니다. 따라서 디렉토리가 존재하면 "디렉토리가 존재합니다."를 출력하고, 존재하지 않으면 "디렉토리가 존재하지 않습니다."를 출력합니다.

## 더 들어가기
FileManager의 `fileExists(atPath:)` 메서드는 디렉토리가 존재하는지 여부만을 판단할 뿐만 아니라, 해당 디렉토리가 파일인지 아닌지도 구분할 수 있습니다. 만약 디렉토리가 파일이라면 `isDirectory` 속성을 이용하여 아래와 같이 분기처리할 수 있습니다.

```Swift
let fileManager = FileManager.default
let path = "/Users/example/Documents/New Folder" // 디렉토리 경로 지정
var isDirectory = ObjCBool(false) // isDirectory 변수 선언
if fileManager.fileExists(atPath: path, isDirectory: &isDirectory) {
  if isDirectory.boolValue { // 디렉토리일 경우
    print("해당 경로는 디렉토리입니다.")
  } else { // 파일일 경우 
    print("해당 경로는 파일입니다.")
  }
} else {
  print("해당 경로는 존재하지 않습니다.")
}
```

위의 코드에서는 `isDirectory` 변수를 선언하고, `fileExists(atPath:isDirectory:)` 메서드를 사용하여 해당 경로가 파일인지 디렉토리인지를 판단합니다. 그리고 이에 따라 적절한 메시지가 출력됩니다. 이를 통해 디렉토리가 존재하는지 여부뿐만 아니라, 해당 디렉토리가 파일인지 디렉토리인지도 구분할 수 있습니다.

## 여기서 더 알아보기
- [Apple Developer Documentation - File Manager](https://developer.apple.com/documentation/foundation/filemanager)
- [Hacking with Swift - How to check if a file exists using FileManager](https://www.hackingwithswift.com/example-code/system/how-to-check-if-a-file-exists-using-filemanager)
- [Swift by Sundell - Working with the file system in Swift](https://www.swiftbysundell.com/articles/working-with-the-file-system-in-swift/)