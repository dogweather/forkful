---
date: 2024-01-20 17:35:43.729874-07:00
description: "How to: (\uBC29\uBC95) Swift\uC5D0\uC11C \uBB38\uC790\uC5F4 \uC5F0\uACB0\
  \uC740 \uB9E4\uC6B0 \uC9C1\uAD00\uC801\uC785\uB2C8\uB2E4. '+' \uC5F0\uC0B0\uC790\
  \uB97C \uC0AC\uC6A9\uD558\uAC70\uB098 \uBB38\uC790\uC5F4 \uBCF4\uAC04 \uAE30\uB2A5\
  \uC744 \uC774\uC6A9\uD574 \uBD05\uC2DC\uB2E4."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.338328-06:00'
model: gpt-4-1106-preview
summary: "(\uBC29\uBC95) Swift\uC5D0\uC11C \uBB38\uC790\uC5F4 \uC5F0\uACB0\uC740 \uB9E4\
  \uC6B0 \uC9C1\uAD00\uC801\uC785\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uC5F0\uACB0\uD558\uAE30"
weight: 3
---

## How to: (방법)
Swift에서 문자열 연결은 매우 직관적입니다. '+' 연산자를 사용하거나 문자열 보간 기능을 이용해 봅시다.

```swift
// '+' 연산자로 문자열 연결
let firstName = "홍"
let lastName = "길동"
let fullName = firstName + " " + lastName
print(fullName) // "홍 길동" 출력

// 문자열 보간으로 문자열 연결
let age = 20
let introduction = "제 이름은 \(fullName)이고, 나이는 \(age)살입니다."
print(introduction) // "제 이름은 홍 길동이고, 나이는 20살입니다." 출력
```

## Deep Dive (심화 탐구)
Swift가 처음 등장했을 때부터 문자열 연결은 기본적인 기능으로 자리잡았습니다. '+' 연산자를 사용하거나 문자열 보간법은 Swift가 제공하는 두 가지 주요 방법입니다.

문자열 보간법은 Swift의 강력한 기능 중 하나로, 변수나 상수, 심지어 전체 표현식을 괄호로 묶고 그 앞에 역슬래시를 붙여 문자열에 삽입합니다. 복잡한 형태의 데이터를 문자열로 표현할 때 매우 유용하죠.

하지만 많은 양의 문자열을 반복해서 연결해야 한다면 '+' 연산자 방식은 비효율적일 수 있습니다. 이럴 때는 `Array`의 `joined()` 메서드나 `String`의 `appending()` 메서드를 사용하는 것이 좋습니다. 반복 처리에서는 문자열 빌더 패턴을 사용하는 것이 성능상 이점을 가질 수 있습니다.

예를 들어, 배열의 문자열 항목들을 하나의 문자열로 합치는 경우에는 다음과 같이 합니다.

```swift
// joined 메서드 사용
let words = ["Swift", "프로그래밍", "재밌다"]
let sentence = words.joined(separator: " ")
print(sentence) // "Swift 프로그래밍 재밌다" 출력
```

Swift에서 문자열은 `String` 타입으로 표현되며, 이는 '값 타입(Value Type)'이라 할당 또는 함수에 전달할 때마다 복사된다는 특징이 있습니다. 그래서 큰 문자열 연산을 자주 하면 성능이 저하될 수 있어 이점 유의해야 합니다.

## See Also (참고 자료)
- [Swift 공식 문자열 처리 가이드](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Apple's Swift API for Strings](https://developer.apple.com/documentation/swift/string)
- [Swift Programming: The Big Nerd Ranch Guide](https://www.bignerdranch.com/books/swift-programming/) - 문자열 조작에 대한 좀 더 심도있는 설명이 있습니다.
