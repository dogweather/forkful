---
date: 2024-01-20 17:52:06.965264-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uC0AC\uC6A9\uD558\uB098\uC694?) Swift \uCF54\
  \uB4DC\uC5D0\uC11C \uBB38\uC790\uC5F4 \uBCF4\uAC04\uC744 \uC0AC\uC6A9\uD558\uB294\
  \ \uC608\uC2DC\uB4E4\uC785\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.331619-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C \uC0AC\uC6A9\uD558\uB098\uC694?) Swift \uCF54\uB4DC\uC5D0\
  \uC11C \uBB38\uC790\uC5F4 \uBCF4\uAC04\uC744 \uC0AC\uC6A9\uD558\uB294 \uC608\uC2DC\
  \uB4E4\uC785\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uBCF4\uAC04\uD558\uAE30"
weight: 8
---

## How to: (어떻게 사용하나요?)
Swift 코드에서 문자열 보간을 사용하는 예시들입니다.

```Swift
let name = "홍길동"
let greeting = "안녕하세요, \(name)님!"
print(greeting)
// 출력: 안녕하세요, 홍길동님!

let temperature = 23.5
let weatherMessage = "오늘 기온은 \(temperature)도 입니다."
print(weatherMessage)
// 출력: 오늘 기온은 23.5도 입니다.

let apples = 3
let oranges = 5
let fruitSummary = "나는 사과를 \(apples)개, 오렌지를 \(oranges)개 가지고 있어요."
print(fruitSummary)
// 출력: 나는 사과를 3개, 오렌지를 5개 가지고 있어요.
```

## Deep Dive (더 알아보기)
Swift에 도입된 문자열 보간은 코드의 간결성과 유지보수를 돕습니다. 이전에는 문자열을 복잡하게 연결(concatenation)해야 했지만, Swift의 문자열 보간을 사용하면 직관적으로 변수를 삽입할 수 있습니다. 파이썬이나 루비 같은 다른 언어에도 비슷한 특징이 있습니다만, Swift는 타입 안전성(type safety)을 강조하여 컴파일 시점에 오류를 잡을 수 있도록 합니다. 보간된 문자열은 실제로 `String` 타입의 인스턴스로, 실행 시 해당 부분이 값으로 치환됩니다.

## See Also (더 참고할 자료)
- [Swift Documentation: String Interpolation](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Swift Programming from Scratch](https://www.weheartswift.com/swift-programming-scratch-100-exercises/)
