---
date: 2024-01-20 17:55:10.575496-07:00
description: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC77D\uAE30\uB294 \uB514\uC2A4\uD06C\
  \uC5D0 \uC800\uC7A5\uB41C \uD14D\uC2A4\uD2B8 \uB370\uC774\uD130\uB97C \uC77D\uACE0\
  \ Swift \uD504\uB85C\uADF8\uB7A8\uC5D0\uC11C \uC0AC\uC6A9\uD558\uB294 \uAC83\uC785\
  \uB2C8\uB2E4. \uD30C\uC77C\uC5D0\uC11C \uC124\uC815, \uC0AC\uC6A9\uC790 \uB370\uC774\
  \uD130, \uB610\uB294 \uB300\uB7C9\uC758 \uD14D\uC2A4\uD2B8\uB97C \uC77D\uC5B4\uC57C\
  \ \uD560 \uB54C \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC774 \uC0AC\uC6A9\uD569\uB2C8\
  \uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.756152-06:00'
model: gpt-4-1106-preview
summary: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC77D\uAE30\uB294 \uB514\uC2A4\uD06C\uC5D0\
  \ \uC800\uC7A5\uB41C \uD14D\uC2A4\uD2B8 \uB370\uC774\uD130\uB97C \uC77D\uACE0 Swift\
  \ \uD504\uB85C\uADF8\uB7A8\uC5D0\uC11C \uC0AC\uC6A9\uD558\uB294 \uAC83\uC785\uB2C8\
  \uB2E4. \uD30C\uC77C\uC5D0\uC11C \uC124\uC815, \uC0AC\uC6A9\uC790 \uB370\uC774\uD130\
  , \uB610\uB294 \uB300\uB7C9\uC758 \uD14D\uC2A4\uD2B8\uB97C \uC77D\uC5B4\uC57C \uD560\
  \ \uB54C \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC774 \uC0AC\uC6A9\uD569\uB2C8\uB2E4\
  ."
title: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC77D\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
텍스트 파일 읽기는 디스크에 저장된 텍스트 데이터를 읽고 Swift 프로그램에서 사용하는 것입니다. 파일에서 설정, 사용자 데이터, 또는 대량의 텍스트를 읽어야 할 때 프로그래머들이 사용합니다.

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
