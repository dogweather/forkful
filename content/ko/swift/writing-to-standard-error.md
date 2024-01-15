---
title:                "표준 오류로 작성하기"
html_title:           "Swift: 표준 오류로 작성하기"
simple_title:         "표준 오류로 작성하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜
누군가가 표준 오류로 쓰기에 참여하는 이유에 대해 최대 2문장으로 설명합니다.

표준 오류로 쓰기는 프로그래밍에서 오류를 디버깅하는 데 매우 유용한 도구입니다. 오류가 발생한 경우 이를 빠르게 식별하고 수정할 수 있도록 합니다. 또한 프로그램의 실행 상태를 실시간으로 모니터링할 수 있습니다.

## 방법
코드 블록인"```Swift ... ```" 안에 코드 예제와 샘플 출력을 포함하여 설명합니다.

```Swift
import Foundation

// 표준 오류로 문자열 출력하기
let errorMessage = "오류가 발생했습니다."
FileHandle.standardError.write(errorMessage.data(using: .utf8)!)

// 표준 오류로 정수 출력하기
let errorNumber = 404
FileHandle.standardError.write("\(errorNumber)".data(using: .utf8)!)
```

출력:
```
404
```

## 심화 학습
표준 오류에 대해 더 깊이있는 정보를 제공합니다.

표준 오류는 프로그램에서 발생한 오류를 캡쳐하고 콘솔에 출력하는 데 사용됩니다. 이것은 표준 출력과는 달리 오류를 따로 처리하여 디버깅을 더 쉽게 만듭니다. 또한 표준 오류를 파일로 리디렉션하면 오류 내용을 나중에 확인할 수 있습니다.

## 관련 링크
- [Swift Documentation - FileHandle](https://developer.apple.com/documentation/foundation/filehandle)
- [How to write to stderr in Swift?](https://stackoverflow.com/questions/28933352/how-to-write-to-stderr-in-swift)
- [Debugging with Stderr in Swift](https://joemasilotti.com/Debugging-With-Stderr-in-Swift/)