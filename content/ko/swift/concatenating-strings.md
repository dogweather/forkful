---
title:    "Swift: 문자열 연결하기"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## 왜
문자열 연결을 할 필요가 있는 이유는 개발자가 여러 문자열을 하나의 문자열로 함께 사용하고 싶어서입니다.

## 방법
우선 사용할 변수 또는 상수를 선언합니다. 그리고 그 안에 문자열을 넣어줍니다. 예를 들어, name 변수에 "제이슨"을 넣고, number 상수에는 "3" 이라는 문자열을 넣습니다.

```Swift
var name = "제이슨"
let number = "3"
```

그리고 문자열을 연결할 때 사용되는 "+" 기호를 이용하여 변수를 함께 연결해줍니다. 그리고 새로운 변수에 넣어주면 됩니다.

```Swift
var combinedString = name + number
```
이제 이 새로운 변수인 ```combinedString```을 출력해보면, "제이슨3" 이라는 문자열이 출력됩니다.

```Swift
print(combinedString) // 제이슨3
```

## 딥 다이브
문자열 연결을 위해 Swift에서는 다양한 방법을 제공합니다. 위에서 설명했던 "+" 기호를 이용하는 방법 외에도, ```+``` 기호 대신 ```+=``` 기호를 사용하여 간편하게 변수 내부에 문자열을 연결할 수 있습니다.

또한, Swift는 문자열 보간법이라는 기능을 제공하여 변수 또는 상수를 문자열 안에 바로 사용할 수 있게 해줍니다. 예를 들어 위의 코드에서 "제이슨3" 대신에 ```\(name)\(number)``` 이렇게 사용해도 같은 결과를 얻을 수 있습니다.

또한, 만약 여러 개의 변수를 연결하고 싶다면, ```+``` 기호를 여러 번 사용할 수 있습니다. 그리고 문자열을 연결하는 대신, Swift에서 제공하는 ```joined(_:)``` 메서드를 이용하여 여러 문자열을 한 번에 연결하는 방법도 있습니다.

## 참고 자료
[Swift 공식 문서 - Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)

[Swift 기본문법 정리 - 문자열 보간법](https://jusung.github.io/Swift-기본문법-정리/)

[Swift에서 문자열 연결하는 다양한 방법](https://zeddios.tistory.com/104)

## 더 보기