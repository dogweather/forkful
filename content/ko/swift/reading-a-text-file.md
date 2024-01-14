---
title:                "Swift: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

# 왜

텍스트 파일을 읽는 방법에 대해 배우기 전에, 우리는 왜 텍스트 파일을 읽는 것이 중요한지에 대해 이야기해야 합니다. 텍스트 파일을 읽는 것은 스위프트 프로그래밍에서 중요한 기능 중 하나입니다. 그 이유는 우리가 컴퓨터에 저장된 데이터를 읽고 처리하기 위해서는 텍스트 파일의 정보를 읽는 것이 필수적이기 때문입니다.

# 어떻게

먼저, 우리는 `Foundation` 프레임워크에서 제공하는 `String` 타입의 `init(contentsOf:usedEncoding:)` 메서드를 사용하여 텍스트 파일을 읽을 수 있습니다. 이 메서드는 텍스트 파일을 `String` 타입으로 변환해주는데, 파일의 경로와 인코딩 방식을 지정할 수 있습니다. 다음은 이 메서드를 사용한 예시입니다.

```Swift
let fileURL = URL(fileURLWithPath: "path/to/file.txt")
do {
    let fileContents = try String(contentsOf: fileURL, usedEncoding: .utf8)
    print(fileContents)
} catch {
    print("파일을 읽는 데 실패하였습니다.")
}
```

위의 코드에서는 `do-catch` 문법을 사용하여 파일을 읽다가 발생할 수 있는 오류를 처리해주었습니다. 만약 파일을 성공적으로 읽는다면, `fileContents`에 파일의 내용이 저장되어 출력될 것입니다. 만약 파일을 읽는 데 실패하면, 오류 메시지가 출력될 것입니다.

# 딥 다이브

`String` 타입의 `init(contentsOf:usedEncoding:)` 메서드 외에도, `FileHandle` 클래스를 사용하여 좀 더 효율적으로 텍스트 파일을 읽을 수 있습니다. 이 클래스는 파일에 대한 접근을 제공하며, `Data` 타입으로 파일의 내용을 읽어올 수 있습니다. 다음은 `FileHandle` 클래스를 사용하여 텍스트 파일을 읽는 예시입니다.

```Swift
let fileURL = URL(fileURLWithPath: "path/to/file.txt")
let fileHandle = FileHandle(forReadingAtPath: fileURL.path)
if let fileContents = fileHandle?.readDataToEndOfFile(), let contentsAsString = String(data: fileContents, encoding: .utf8) {
    print(contentsAsString)
} else {
    print("파일을 읽는 데 실패하였습니다.")
}
```

위의 코드에서는 `FileHandle`의 `readDataToEndOfFile()` 메서드를 사용하여 파일의 내용을 `Data` 타입으로 읽어왔습니다. 그 후 `String` 타입으로 변환하여 출력하였습니다. 이 방법은 더 많은 양의 데이터를 효율적으로 처리할 수 있습니다.

# 참고

- [String - Apple Developer Documentation](https://developer.apple.com/documentation/foundation/string)
- [FileHandle - Apple Developer Documentation](https://developer.apple.com/documentation/foundation/filehandle)