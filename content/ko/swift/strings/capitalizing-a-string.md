---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:44.128540-07:00
description: "Swift\uC5D0\uC11C \uBB38\uC790\uC5F4\uC758 \uCCAB \uAE00\uC790\uB97C\
  \ \uB300\uBB38\uC790\uB85C, \uB098\uBA38\uC9C0 \uAE00\uC790\uB4E4\uC744 \uC18C\uBB38\
  \uC790\uB85C \uBCC0\uD615\uC2DC\uD0A4\uB294 \uAC83\uC744 \uBB38\uC790\uC5F4 \uB300\
  \uBB38\uC790\uD654\uB77C\uACE0 \uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uBB38\uBC95 \uADDC\uCE59\uC774\uB098 \uC0AC\uC6A9\uC790 \uC778\uD130\
  \uD398\uC774\uC2A4 \uD45C\uC900\uC5D0 \uB530\uB77C \uC774\uB984\uC774\uB098 \uBB38\
  \uC7A5\uC744 \uD615\uC2DD\uD654\uD558\uB294 \uBAA9\uC801 \uB4F1\uC73C\uB85C \uC774\
  \uB97C \uC218\uD589\uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.707061-06:00'
model: gpt-4-0125-preview
summary: "Swift\uC5D0\uC11C \uBB38\uC790\uC5F4\uC758 \uCCAB \uAE00\uC790\uB97C \uB300\
  \uBB38\uC790\uB85C, \uB098\uBA38\uC9C0 \uAE00\uC790\uB4E4\uC744 \uC18C\uBB38\uC790\
  \uB85C \uBCC0\uD615\uC2DC\uD0A4\uB294 \uAC83\uC744 \uBB38\uC790\uC5F4 \uB300\uBB38\
  \uC790\uD654\uB77C\uACE0 \uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uB300\uBB38\uC790\uD654"
weight: 2
---

## 방법:
Swift의 `String` 구조체는 문자열의 대소문자를 조작하기 위한 내장 메소드를 몇 가지 가지고 있습니다. 여기 Swift에서 문자열을 대문자화하는 몇 가지 접근 방법이 있습니다. 표준 메소드 사용 및 필요한 경우 타사 라이브러리 사용이 포함됩니다.

### 내장 메소드 사용하기
문자열의 첫 글자를 대문자로 하고 나머지를 소문자로 변경하려면:

```swift
let myString = "hello, world"
let capitalizedString = myString.prefix(1).uppercased() + myString.dropFirst().lowercased()
print(capitalizedString) // 출력: "Hello, world"
```

문장의 각 단어의 첫 글자를 대문자로 만들려면, `capitalized` 속성을 사용할 수 있습니다:

```swift
let sentence = "hello, world"
let capitalizedSentence = sentence.capitalized
print(capitalizedSentence) // 출력: "Hello, World"
```

### 타사 라이브러리 사용하기
Swift의 표준 라이브러리는 상당히 포괄적이지만, 특정 대문자 형식은 더 복잡한 연산을 요구하거나 타사 라이브러리를 사용하여 간소화될 수 있습니다. 문자열 조작에 있어 인기 있는 라이브러리 중 하나는 SwiftRichString입니다. (참고: 항상 Swift 패키지 관리자, CocoaPods 또는 Carthage를 통해 타사 라이브러리를 포함시키고 파일에서 임포트하십시오.)

먼저, 프로젝트에 `SwiftRichString`을 추가해야 합니다. 설치되면, 특정 대문자화 필요성을 포함하여 다양한 문자열 작업을 수행하는 데 사용할 수 있습니다. 그러나 현재로써는, Swift의 내장 메소드만으로도 대부분의 대문자화 사용 사례를 충분히 커버할 수 있어, 문자열을 대문자화하기 위해 외부 라이브러리를 필요로 하지 않습니다.

메소드의 업데이트나 변경 사항에 대해 항상 라이브러리의 최신 문서를 참조하십시오.
