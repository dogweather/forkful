---
date: 2024-01-20 17:39:27.644022-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) \uBB38\uC790\uC5F4\uC744\
  \ \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\uD558\uB294 \uAE30\uB2A5\uC740 \uAC70\uC758\
  \ \uBAA8\uB4E0 \uD504\uB85C\uADF8\uB798\uBC0D \uC5B8\uC5B4\uC5D0 \uAD6C\uD604\uB418\
  \uC5B4 \uC788\uC2B5\uB2C8\uB2E4. \uC2A4\uC704\uD504\uD2B8\uC5D0\uC11C `.lowercased()`\
  \ \uBA54\uC18C\uB4DC\uB294 Unicode\uC758 \uC815\uC758\uB97C \uB530\uB77C \uC5B4\uB5A4\
  \ \uBB38\uC790\uAC00 \uC18C\uBB38\uC790\uC778\uC9C0 \uACB0\uC815\uD569\uB2C8\uB2E4\
  . \uC774\uB294 \uB2E4\uC591\uD55C \uC5B8\uC5B4\uC640 \uBB38\uC790\uB97C \uCC98\uB9AC\
  \uD560 \uC218 \uC788\uAC8C\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:09.952290-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) \uBB38\uC790\uC5F4\uC744 \uC18C\
  \uBB38\uC790\uB85C \uBCC0\uD658\uD558\uB294 \uAE30\uB2A5\uC740 \uAC70\uC758 \uBAA8\
  \uB4E0 \uD504\uB85C\uADF8\uB798\uBC0D \uC5B8\uC5B4\uC5D0 \uAD6C\uD604\uB418\uC5B4\
  \ \uC788\uC2B5\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\uD558\uAE30"
weight: 4
---

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
