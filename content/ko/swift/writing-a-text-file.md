---
title:    "Swift: 텍스트 파일 작성하기"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## 왜
텍스트 파일을 작성하는 이유는 프로그램에서 데이터를 저장하고 사용하는데 유용하기 때문입니다.

## 하는 방법 
```Swift
let text = "안녕하세요! 반가워요!"
let fileURL = URL(fileURLWithPath: "/Users/username/Desktop/HelloWorld.txt")
do {
    try text.write(to: fileURL, atomically: false, encoding: .utf8)
} catch {
    print(error)
}
```

위의 코드는 스위프트에서 텍스트 파일을 만드는 간단한 예시입니다. `text` 변수에 쓰여진 내용을 `fileURL` 경로에 있는 파일에 작성합니다. `try` 문을 사용하여 작업이 잘 수행되는지 확인하고 `catch`로 에러를 처리합니다.

출력 결과는 `HelloWorld.txt` 파일에 다음과 같이 저장됩니다.

`안녕하세요! 반가워요!`

## 심화 공부 
### 파일에 추가 쓰기
만약 기존의 파일에 추가로 내용을 쓰고 싶다면 `append` 메소드를 사용합니다.

```Swift
let newText = "만나서 반가워요!"
let fileURL = URL(fileURLWithPath: "/Users/username/Desktop/HelloWorld.txt")
do {
    let fileHandle = try FileHandle(forWritingTo: fileURL)
    fileHandle.seekToEndOfFile()
    fileHandle.write(newText.data(using: .utf8)!)
    fileHandle.closeFile()
} catch {
    print(error)
}
```

`fileHandle` 객체를 사용하여 파일의 끝에 `newText` 변수에 저장된 내용을 추가합니다.

### 파일에서 읽기
작성된 텍스트 파일을 읽어오기 위해서는 `String`의 `init(contentsOf:encoding:)` 메소드를 사용합니다.

```Swift
let fileURL = URL(fileURLWithPath: "/Users/username/Desktop/HelloWorld.txt")
do {
    let contents = try String(contentsOf: fileURL, encoding: .utf8)
    print(contents)
} catch {
    print(error)
}
```

출력 결과는 `HelloWorld.txt` 파일의 내용이 그대로 출력됩니다.

## 참고하기 
- [Swift Language Guide - Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Swift Language Guide - Error Handling](https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html)
- [Apple Developer Documentation - Writing Files to a Location](https://developer.apple.com/documentation/foundation/filemanager/1412643-createfile)
- [Apple Developer Documentation - Reading Data from a File](https://developer.apple.com/documentation/foundation/filemanager/1540699-contents)