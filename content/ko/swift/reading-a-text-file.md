---
title:                "텍스트 파일 읽기"
html_title:           "Bash: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 무엇인가요 & 왜 필요한가요? (What & Why?)

텍스트 파일 읽기는 컴퓨터에 저장된 텍스트 파일의 데이터를 불러오는 과정입니다. 프로그래머가 이를 수행하는 이유는 데이터 분석, 파일 수정 및 재사용 등 다양합니다.

## 어떻게 하나요? (How to:)

Swift에서는 다음과 같이 String의 `init(contentsOfFile:)` 또는 `init(contentsOf:encoding:)` 메서드를 사용하여 텍스트 파일을 읽을 수 있습니다.

```Swift
import Foundation

do {
    let fileUrl = try FileManager.default.url(for: .documentDirectory, 
        in: .userDomainMask, appropriateFor: nil, create: false)
        .appendingPathComponent("SampleFile.txt")

    let fileContents = try String(contentsOf: fileUrl, encoding: .utf8)
    print(fileContents)
} catch {
    print("Error: \(error)")
}
```
이 코드는 "SampleFile.txt"라는 텍스트 파일의 내용을 읽어 화면에 출력합니다. 오류가 발생하면 해당 오류 메시지를 출력합니다.

## 깊게 탐색하기 (Deep Dive)

텍스트 파일을 읽는 방법은 예전부터 있었으며 언어마다 다른 방법을 사용합니다. Swift에서는 위에 언급된 메서드 외에도 `NSData`와 같은 다른 클래스들을 사용해서 원시 데이터 형태로 파일을 읽을 수 있습니다. 그러나 일반적인 텍스트 파일을 다루는 것이라면 문자열로 처리하는 것이 가장 간단하고 편리합니다.

값을 읽어올 때는 항상 올바른 인코딩을 사용해야 합니다. 대부분의 경우 `.utf8` 인코딩이 사용되지만, 파일이 만들어진 방식에 따라 다른 인코딩이 필요할 수도 있습니다. 잘못된 인코딩을 사용하면 파일 읽기가 실패하거나 제대로된 데이터를 얻지 못할 수 있습니다.

## 참고 자료 (See Also)

더 많은 정보를 원하신다면 다음 참고 자료들을 확인해보세요:
- [Apple Developer Documentation](https://developer.apple.com/documentation)
- [Swift String API](https://developer.apple.com/documentation/swift/string)
- [Swift File Management Tutorial](https://www.raywenderlich.com/663-filemanager-class-tutorial-for-macos-getting-started)