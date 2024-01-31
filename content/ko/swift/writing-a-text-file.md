---
title:                "텍스트 파일 작성하기"
date:                  2024-01-19
html_title:           "Arduino: 텍스트 파일 작성하기"
simple_title:         "텍스트 파일 작성하기"

category:             "Swift"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
텍스트 파일 쓰기란 문자 데이터를 일반 텍스트 형식으로 파일에 저장하는 행위입니다. 프로그래머는 데이터 로깅, 설정 저장, 사용자 데이터 출력 등을 위해 사용합니다.

## How to: (어떻게 하나)
```Swift
import Foundation

let stringToWrite = "안녕하세요! 파일에 쓰기 예제입니다."
let filePath = NSString(string: "~/Desktop/example.txt").expandingTildeInPath

do {
    try stringToWrite.write(toFile: filePath, atomically: true, encoding: String.Encoding.utf8)
    print("파일 쓰기 성공")
} catch {
    print("파일 쓰기 실패: \(error)")
}
```
## Deep Dive (심층 분석)
- **역사적 맥락**: 초기 컴퓨팅 시대부터 데이터 저장은 필수적이었으며, 텍스트 파일은 가장 기본적인 형태 중 하나입니다.
- **대안들**: 데이터베이스, JSON, XML 파일 혹은 `UserDefaults`와 같은 시스템이 텍스트 파일 대신 사용될 수 있습니다.
- **구현 세부 사항**: Swift에서는 'Foundation' 프레임워크의 `String` 클래스를 사용해 쉽게 파일 쓰기를 할 수 있으며, 에러처리를 위한 `try-catch` 구문을 사용합니다.

## See Also (함께 보기)
- 더 깊은 파일 처리 방법: [File System Basics](https://developer.apple.com/library/archive/documentation/FileManagement/Conceptual/FileSystemProgrammingGuide/FileSystemOverview/FileSystemOverview.html)
- 에러 처리에 대해: [Error Handling in Swift](https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html)
