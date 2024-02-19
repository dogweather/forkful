---
aliases:
- /ko/swift/converting-a-string-to-lower-case/
date: 2024-01-20 17:39:27.644022-07:00
description: "\uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\uD558\
  \uB294 \uAC83\uC740, \uB300\uC18C\uBB38\uC790\uB97C \uAD6C\uBD84\uD558\uC9C0 \uC54A\
  \uB294 \uBE44\uAD50\uB97C \uAC00\uB2A5\uD558\uAC8C \uD558\uAC70\uB098, \uC0AC\uC6A9\
  \uC790\uB4E4\uC5D0\uAC8C \uC77C\uAD00\uC131 \uC788\uB294 \uD14D\uC2A4\uD2B8 \uD615\
  \uD0DC\uB97C \uC81C\uACF5\uD558\uAE30 \uC704\uD574 \uC0AC\uC6A9\uD569\uB2C8\uB2E4\
  ."
isCJKLanguage: true
lastmod: 2024-02-18 23:09:06.728958
model: gpt-4-1106-preview
summary: "\uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\uD558\uB294\
  \ \uAC83\uC740, \uB300\uC18C\uBB38\uC790\uB97C \uAD6C\uBD84\uD558\uC9C0 \uC54A\uB294\
  \ \uBE44\uAD50\uB97C \uAC00\uB2A5\uD558\uAC8C \uD558\uAC70\uB098, \uC0AC\uC6A9\uC790\
  \uB4E4\uC5D0\uAC8C \uC77C\uAD00\uC131 \uC788\uB294 \uD14D\uC2A4\uD2B8 \uD615\uD0DC\
  \uB97C \uC81C\uACF5\uD558\uAE30 \uC704\uD574 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\uD558\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열을 소문자로 변환하는 것은, 대소문자를 구분하지 않는 비교를 가능하게 하거나, 사용자들에게 일관성 있는 텍스트 형태를 제공하기 위해 사용합니다.

## How to: (어떻게 하나요?)
```swift
let greeting = "Hello, World!"
let lowercasedGreeting = greeting.lowercased()
print(lowercasedGreeting)  // "hello, world!"
```
Sample Output:
```
hello, world!
```

```swift
let koreanText = "안녕하세요!"
let lowercasedKoreanText = koreanText.lowercased()
print(lowercasedKoreanText)  // "안녕하세요!"
```
Sample Output:
```
안녕하세요!
```

## Deep Dive (심층 분석)
문자열을 소문자로 변환하는 기능은 거의 모든 프로그래밍 언어에 구현되어 있습니다. 스위프트에서 `.lowercased()` 메소드는 Unicode의 정의를 따라 어떤 문자가 소문자인지 결정합니다. 이는 다양한 언어와 문자를 처리할 수 있게 해줍니다. 예를 들어, 라틴 알파벳은 간단하지만, 특별한 규칙을 가진 다른 언어들도 있습니다. 대안적으로, 개발자는 `String` extension을 이용해 커스텀 소문자 변환 로직을 구현할 수도 있습니다. 그러나 대부분의 경우, Swift의 표준 라이브러리 제공 메소드가 충분합니다. 성능 측면에서, 소문자로 변환은 비교적 리소스가 많이 드는 작업일 수 있으므로, 효율성을 고려해 이를 필요한 경우에만 사용해야 합니다.

## See Also (참고자료)
- Swift Documentation for Strings: [Apple Developer Documentation](https://developer.apple.com/documentation/swift/string) 
- Unicode Standard for Casing: [Unicode Standard, Section 3.13](http://unicode.org/versions/Unicode13.0.0/ch03.pdf#G33992)
