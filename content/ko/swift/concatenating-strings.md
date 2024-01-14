---
title:    "Swift: 문자열 연결"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜
굉장히 간단한 작업에 보기 좋은 출력을 제공해주는 string concatenation이 왜 중요한지 알아보겠습니다.


## 하는 법
string concatenation을 위해서는 "+" 연산자를 사용하거나, String Interpolation을 사용하면 됩니다.

```Swift
// "+" 연산자 사용 예제
let firstName = "민지"
let lastName = "김"

let fullName = firstName + " " + lastName
print(fullName) // 민지 김

// String Interpolation 사용 예제
let weight = 55.5
let height = 165

let message = "제 몸무게는 \(weight)kg이고 키는 \(height)cm입니다."
print(message) // 제 몸무게는 55.5kg이고 키는 165cm입니다.
```

## 더 들어가보기
Swift에서만 사용되는 string concatenation 방식에는 두 가지 종류가 있습니다. 첫 번째는 "+" 연산자를 사용하는 방식이고, 두 번째는 String Interpolation 방식입니다.

"+" 연산자를 사용할 때는 변수와 문자열을 더해주어야 하기 때문에, 변수가 많을 경우 코드가 복잡해질 수 있습니다. 하지만 String Interpolation을 사용하면 변수만 {} 안에 넣어주면 됩니다. 이렇게 사용하면 코드가 간결해지고 가독성이 좋아집니다.

## 참고하기
- [Swift 공식 문서 - 문자열과 문자](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Swift 4.0 문서 - 문자열 반복하기](https://developer.apple.com/documentation/swift/string/2427940-init)
- [`String` 문서](https://developer.apple.com/reference/swift/string)