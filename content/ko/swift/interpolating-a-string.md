---
title:                "문자열 보간하기"
date:                  2024-01-20T17:52:06.965264-07:00
model:                 gpt-4-1106-preview
simple_title:         "문자열 보간하기"

category:             "Swift"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가요? 왜 사용하나요?)
문자열 보간(string interpolation)은 변수나 상수, 표현식의 값을 문자열 내부에 삽입하는 방법입니다. 프로그래머들은 코드 가독성을 높이고, 문자열을 쉽게 조작하기 위해 이를 사용합니다.

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
