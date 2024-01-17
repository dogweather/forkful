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

## What & Why?

임시 파일을 만드는 것은 프로그래머들이 자주 하는 일입니다. 임시 파일은 일시적으로 사용될 때 유용하며, 새로운 데이터를 만들거나 기존 데이터를 수정할 때 사용됩니다. 임시 파일은 일반적으로 메모리나 디스크 공간을 절약하고, 작업 속도를 높이기 위해 사용됩니다.

## How to:
임시 파일을 만드는 방법은 간단합니다. Swift에서는 FileManager 클래스를 사용하여 임시 파일을 생성할 수 있습니다. 다음은 임시 파일을 만들고 출력하는 간단한 예제입니다.

```Swift
import Foundation

let fileManager = FileManager()
let tempFileURL = fileManager.temporaryDirectory.appendingPathComponent("tempFile.txt")

do {
    try "Hello World!".write(to: tempFileURL, atomically: true, encoding: .utf8)
    let tempFileContent = try String(contentsOf: tempFileURL, encoding: .utf8)
    print(tempFileContent) // Prints "Hello World!"
} catch {
    print("Failed to create temporary file")
}
```

## Deep Dive:
1. 임시 파일은 1960년대부터 사용되어 온 개념입니다. 초기에는 메모리 상에 저장되는 가상 파일로 사용되었고, 나중에는 디스크 상에 저장되는 실제 파일로 발전하였습니다.
2. 다른 언어에서는 임시 파일을 만드는데에 tmpfile() 함수를 사용할 수 있지만, Swift는 파일을 생성할 때 경로를 지정해야 합니다.
3. 임시 파일은 앱이 실행되는 동안에만 존재하며 앱 종료 시 자동으로 삭제됩니다.

## See Also:
https://developer.apple.com/documentation/foundation/filemanager
https://www.brianstorti.com/understanding-temporary-files-in-ruby/