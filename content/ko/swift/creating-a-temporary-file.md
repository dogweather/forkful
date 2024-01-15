---
title:                "임시 파일 생성하기"
html_title:           "Swift: 임시 파일 생성하기"
simple_title:         "임시 파일 생성하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜

임시 파일을 생성하는 것에 참여하는 이유는 매우 간단합니다. 이는 파일을 생성하고 사용한 후에 해당 파일을 자동으로 삭제하게 해주기 때문입니다. 이는 우리가 불필요한 파일을 수동으로 삭제하는 수고를 덜어줍니다!

## 사용 방법

코드를 작성하기 전에, `FileManager`를 import 해야 합니다. 그러고난 후에는 `FileManager`의 `URLForDirectory` 메소드를 사용하는데, 이는 우리가 생성한 임시 파일을 저장할 디렉토리를 반환합니다. 이후에는 `FileManager`의 `createFile` 메소드를 사용하여 임시 파일을 생성할 수 있습니다.

``` Swift 
let fileManager = FileManager.default
let tempDir = fileManager.URLForDirectory(FileManager.SearchPathDirectory.ItemReplacementDirectory, inDomain: FileManager.SearchPathDomainMask.UserDomainMask, appropriateForURL: nil, create: true, error: nil)
let tempURL = tempDir?.appendingPathComponent("temp.txt")

do {
    try "This is a temporary file!".write(to: tempURL!, atomically: true, encoding: String.Encoding.utf8)
    print("\(tempURL!) 파일이 생성되었습니다.")
} catch {
    print("파일을 생성하는데 실패했습니다.")
}
```

위의 코드를 실행하면 다음과 같은 출력을 볼 수 있습니다.

```
file:///Users/username/Library/Temporary%20Items/temp.txt 파일이 생성되었습니다.
```

## 깊이 들어가기

임시 파일을 생성하는데는 두 가지 방법이 있습니다. 첫 번째 방법은 `FileManager`의 `URLForDirectory` 메소드를 사용하는 것이고, 두 번째 방법은 `NSTemporaryDirectory` 메소드를 사용하는 것입니다. 두 방법 중 어떤 것을 사용하든 임시 파일을 생성하는 것은 동일하지만, `NSTemporaryDirectory`는 iOS 7 이상에서만 사용 가능합니다.

## 참고 자료

- [Apple's Documentation on NSTemporaryDirectory](https://developer.apple.com/documentation/foundation/1409215-nstemporarydirectory)
- [How To Use FileManager in Swift](https://www.ralfebert.de/ios-examples/foundation/filemanager/)