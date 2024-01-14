---
title:    "Swift: 텍스트 파일 읽기"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

# 왜

텍스트 파일을 읽는 것에 대해 생각해보면 종종 우리는 데이터 분석이나 파일 처리를 위해 이 작업을 해야하는 경우가 있습니다.

# 어떻게

```Swift 
let dataFile = "data.txt"
do {
    let fileContent = try String(contentsOfFile: dataFile)
    print(fileContent)
} catch {
    print("Error: unable to read file")
}
```
**출력 결과:**
```
Hello world!
This is a sample text file.
```

위의 예제 코드를 보면, `try`와 `catch`를 이용하여 파일을 읽는 과정에서 발생할 수 있는 오류를 처리해주고 있습니다. `File` 클래스의 `String` 메소드를 이용하여 파일을 문자열로 변환하고, 해당 문자열을 `print` 함수를 이용하여 출력하고 있습니다. 이 예제를 참고하여, 여러분들도 텍스트 파일을 읽는 코드를 작성해보세요.

# 딥 다이브

텍스트 파일을 읽을 때 다양한 옵션을 설정할 수 있습니다. 대표적인 예시로는 `encoding`과 `options`가 있습니다. `encoding`은 파일의 인코딩 방식을 지정할 수 있고, `options`는 파일을 읽을 때의 동작을 설정할 수 있습니다.

`File` 클래스에 대한 더 자세한 정보는 공식 [문서](https://developer.apple.com/documentation/foundation/filehandle)를 참고하세요.

# 참고

- [Swifty Basics: Reading and Writing Files in Swift](https://www.swiftbysundell.com/basics/filemanager)
- [Read and write text files in Swift](https://www.hackingwithswift.com/example-code/strings/how-to-read-and-write-strings-in-swift)