---
title:    "Swift: 텍스트 파일 작성하기"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 작성하는 이유는 데이터를 저장하고 조작하는 데에 있어서 매우 유용하기 때문입니다.

## 작성하는 방법

```Swift
let text = "안녕하세요! Swift로 텍스트 파일을 작성하는 방법을 알아보겠습니다."
let fileName = "welcome.txt"

//파일 경로 생성
let fileURL = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent(fileName)

//파일에 텍스트 쓰기
do {
    try text.write(to: fileURL, atomically: true, encoding: .utf8)
    print("파일이 성공적으로 작성되었습니다!")
} catch {
    print(error)
}

```

위의 코드를 실행하면, "안녕하세요! Swift로 텍스트 파일을 작성하는 방법을 알아보겠습니다."라는 내용의 파일이 생성됩니다. 코드를 차례대로 살펴보면, 우선 파일 경로를 생성하고 그곳에 작성하고 싶은 내용과 파일명을 변수로 저장합니다. 그리고 파일 경로에 있는 파일에 내용을 쓰기 위해 `write` 메소드를 사용합니다. 이때 `atomically`를 `true`로 설정하면 파일을 안전하게 저장할 수 있습니다. 마지막으로 작성이 완료되면 성공 메시지가 출력됩니다.

## 깊이 파해치기

작성된 텍스트 파일을 다시 사용하기 위해서는 파일이 저장된 경로를 알아야 합니다. 위의 코드에서는 `fileURL` 변수에 경로를 저장했는데, 이 경로를 활용하면 파일을 불러오거나 수정할 수 있습니다. 또한, `FileManager` 클래스를 사용하여 파일을 생성하고 삭제하는 등 다양한 작업을 할 수도 있습니다.

## 더 알아보기

- [Swift official documentation](https://developer.apple.com/documentation/swift)
- [How to Read and Write to a Text File in Swift](https://www.ioscreator.com/tutorials/read-and-write-text-file-swift)
- [Working with Directories in Swift](https://www.hackingwithswift.com/example-code/system/how-to-create-a-directory-if-it-doesnt-already-exist)
- [File Management in Swift with FileManager](https://medium.com/swift-programming/swift-filemanager-87c7e54d54ef)