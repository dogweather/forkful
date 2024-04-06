---
date: 2024-01-20 17:55:10.575496-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C:) Swift\uC5D0\uC11C \uD14D\uC2A4\uD2B8 \uD30C\
  \uC77C\uC744 \uC77D\uB294 \uAE30\uB2A5\uC740 \uCD08\uAE30 Objective-C\uC640 Cocoa\
  \ \uD130\uCE58 \uD504\uB808\uC784\uC6CC\uD06C\uC5D0\uC11C \uC720\uB798\uD569\uB2C8\
  \uB2E4. `String(contentsOf:encoding:)` \uBA54\uC18C\uB4DC\uB294 Swift\uC758 String\
  \ \uD0C0\uC785\uC5D0 \uCD94\uAC00\uB418\uC5C8\uC2B5\uB2C8\uB2E4. \uB300\uC548\uC73C\
  \uB85C\uB294\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.366299-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C:) Swift\uC5D0\uC11C \uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC744\
  \ \uC77D\uB294 \uAE30\uB2A5\uC740 \uCD08\uAE30 Objective-C\uC640 Cocoa \uD130\uCE58\
  \ \uD504\uB808\uC784\uC6CC\uD06C\uC5D0\uC11C \uC720\uB798\uD569\uB2C8\uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC77D\uAE30"
weight: 22
---

## How to: (어떻게:)
```Swift
import Foundation

// 파일 URL 지정
guard let fileURL = Bundle.main.url(forResource: "sample", withExtension: "txt") else {
    fatalError("File not found.")
}

do {
    // 텍스트 파일 읽기
    let contents = try String(contentsOf: fileURL, encoding: .utf8)
    print(contents)
} catch {
    // 에러 처리
    print("File read error: \(error)")
}
```
```
Sample Output:
이것은 샘플 텍스트 파일입니다.
여러 줄에 걸쳐 텍스트가 있을 수 있어요.
```

## Deep Dive (심화 학습)
Swift에서 텍스트 파일을 읽는 기능은 초기 Objective-C와 Cocoa 터치 프레임워크에서 유래합니다. `String(contentsOf:encoding:)` 메소드는 Swift의 String 타입에 추가되었습니다. 대안으로는 `NSData`를 사용하거나, 낮은 수준의 파일 스트림을 직접 다루는 방법이 있습니다. 구현 세부사항으로는 문자 인코딩 문제와 큰 파일을 읽을 때의 메모리 관리가 중요합니다.

## See Also (참고자료)
- [Swift String Documentation](https://developer.apple.com/documentation/swift/string)
- [Swift Book (The Swift Programming Language)](https://docs.swift.org/swift-book/)
