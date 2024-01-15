---
title:                "텍스트 파일 읽기"
html_title:           "Swift: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

이번 글에서는 Swift 프로그래밍 언어를 사용하여 한국 독자들에게 텍스트 파일을 읽는 방법에 대해 알려드릴 것입니다. 텍스트 파일을 읽는 방법을 익히면 프로그래밍 공부에 큰 도움이 될 뿐만 아니라, 다양한 분야에서 자주 사용되는 기술입니다.

## 어떻게

```Swift
// 텍스트 파일의 경로를 지정해줍니다.
let fileURL = Bundle.main.path(forResource: "example", ofType: "txt")
// 파일을 읽을 때는 "try? String(contentsOfFile: fileURL)"를 사용합니다.
if let fileContents = try? String(contentsOfFile: fileURL) {
    // 파일 내용을 출력합니다.
    print(fileContents)
}
```

위 코드를 실행하면 프로젝트 내에 있는 "example.txt" 파일의 내용이 출력됩니다. 파일의 내용을 수정하고 싶다면 "fileContents" 변수에 새로운 값을 할당하면 됩니다. 또한, 파일이 존재하지 않을 때를 대비하여 "if let" 구문을 사용하여 오류를 방지할 수도 있습니다.

## 심층 분석

Swift에서 텍스트 파일을 읽는 방법은 매우 간단합니다. 우선, 파일의 경로를 지정해준 다음 "String(contentsOfFile:)" 메서드를 사용하여 파일의 내용을 문자열로 변환해줍니다. 이때 "try?" 예약어를 사용하여 오류를 방지할 수 있습니다. 이렇게 변환된 문자열을 변수에 할당하면 파일의 내용을 자유롭게 다룰 수 있습니다.

## 또 다른 참고 자료

- [Apple Developer Documentation](https://developer.apple.com/documentation/foundation/nsstring/1411387-initwithcontentsoffile)
- [SwiftLee - Reading From and Writing to a File in Swift](https://www.avanderlee.com/swift/reading-writing-files-with-nsfilemanager/)
- [Hacking with Swift - How to read a text file into a string](https://www.hackingwithswift.com/example-code/language/how-to-read-a-text-file-into-a-string)