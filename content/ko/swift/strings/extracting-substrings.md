---
date: 2024-01-20 17:47:07.387354-07:00
description: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uD2B9\uC815 \uBD80\uBD84\uC744 \uCD94\
  \uCD9C\uD558\uB294 \uAC83\uC740 \uB370\uC774\uD130\uB97C \uC790\uB974\uACE0 \uC870\
  \uC791\uD560 \uB54C \uD544\uC694\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uC6D0\uD558\uB294 \uC815\uBCF4\uB9CC \uC5BB\uAC70\uB098 \uBB38\uC790\
  \uC5F4\uC744 \uBD84\uC11D\uD558\uAE30 \uC704\uD574 \uC774 \uAE30\uB2A5\uC744 \uC790\
  \uC8FC \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: 2024-02-19 22:05:14.625979
model: gpt-4-1106-preview
summary: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uD2B9\uC815 \uBD80\uBD84\uC744 \uCD94\uCD9C\
  \uD558\uB294 \uAC83\uC740 \uB370\uC774\uD130\uB97C \uC790\uB974\uACE0 \uC870\uC791\
  \uD560 \uB54C \uD544\uC694\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\
  \uC740 \uC6D0\uD558\uB294 \uC815\uBCF4\uB9CC \uC5BB\uAC70\uB098 \uBB38\uC790\uC5F4\
  \uC744 \uBD84\uC11D\uD558\uAE30 \uC704\uD574 \uC774 \uAE30\uB2A5\uC744 \uC790\uC8FC\
  \ \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uBD80\uBD84 \uBB38\uC790\uC5F4 \uCD94\uCD9C"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜?)
문자열에서 특정 부분을 추출하는 것은 데이터를 자르고 조작할 때 필요합니다. 프로그래머들은 원하는 정보만 얻거나 문자열을 분석하기 위해 이 기능을 자주 사용합니다.

## How to: (어떻게 하나요?)
Swift에서 문자열의 일부를 추출하는 기본적인 방법들을 살펴봅시다. 아래 예시 코드를 따라 해 보세요.

```Swift
let fullString = "Hello, Swift World!"
let startIndex = fullString.index(fullString.startIndex, offsetBy: 7)
let endIndex = fullString.index(fullString.startIndex, offsetBy: 11)

// Range를 사용해 부분 문자열 추출
let substring1 = fullString[startIndex...endIndex]
print(substring1)  // "Swift"

// 범위 없이 시작과 끝 인덱스 사용
let substring2 = fullString[startIndex..<endIndex]
print(substring2)  // "Swif"

// 특정 문자를 기준으로 분할
if let commaIndex = fullString.firstIndex(of: ",") {
    let substring3 = fullString[..<commaIndex]
    print(substring3)  // "Hello"
}
```

## Deep Dive (심도있게 살펴보기)
Swift에서 문자열을 다루는 작업은 Foundation 프레임워크와 Swift의 표준 라이브러리에 기반하여 발전해왔습니다. 문자열의 불변성(Immutability)과 인코딩의 표준화를 위해 Swift 4부터는 `String` 구조체가 `StringProtocol`을 준수하도록 바뀌었습니다.

앞선 방식 외에도, `NSString` 라이브러리의 메소드를 활용해 좀 더 복잡한 문자열 조작을 할 수 있어요. 또한 정규 표현식을 사용해 문자열을 추출하는 방법도 있지요. 하지만 이 글에서는 더 단순하면서도 스위프트 스러운 방법 위주로 설명하겠습니다.

`Substring`은 문자열에서 임시적으로 나온 부분으로, 메모리를 효율적으로 사용하기 위해 원래 문자열을 참조합니다. 하지만 오랜 시간 동안 저장해두려면 `String`으로 변환하는 것이 좋습니다.

## See Also (더 알아보기)
- [Swift Programming Language Guide - Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Swift API Reference - Substring](https://developer.apple.com/documentation/swift/substring)
