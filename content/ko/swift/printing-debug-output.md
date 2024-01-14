---
title:                "Swift: 디버그 출력 출력"
simple_title:         "디버그 출력 출력"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## 왜

디버그 출력문을 사용하는 이유는 프로그래밍 중에 코드의 실행 상태를 확인하기 위해서입니다.

## 사용 방법

```Swift
// 출력문을 위한 변수 설정
var debugOutput = "디버그 출력문 테스트입니다."
// print 함수를 사용하여 변수의 값을 출력
print(debugOutput)
```

출력 결과:

```
디버그 출력문 테스트입니다.
```

만약 여러 개의 변수의 값을 함께 출력하고 싶다면, 쉼표로 변수를 구분하여 넣어줄 수도 있습니다.

```Swift
var name = "홍길동"
var age = 27
print("이름:", name, "나이:", age)
```

출력 결과:

```
이름: 홍길동 나이: 27
```

## 딥 다이브

디버그 출력문의 더 깊은 이해를 위해 `debugPrint()` 함수를 사용해보겠습니다. 이 함수는 보다 자세한 정보를 제공해주는데, 변수의 데이터 타입과 값을 함께 출력해줍니다.

```Swift
let number = 10
debugPrint(number)
```

출력 결과:

```
10
```

또한 `debugPrint()` 함수는 인자로 `separator`와 `terminator`를 설정할 수 있습니다. `separator`는 각 변수의 값 사이에 들어갈 문자를 정의하는데, 기본값은 공백입니다. `terminator`는 출력 후 마지막에 추가될 문자를 정의하는데, 기본값은 개행 `\n`입니다.

```Swift
let firstName = "John"
let lastName = "Smith"
debugPrint(firstName, lastName, separator: "-", terminator: "!")
```

출력 결과:

```
John-Smith!
```

## 참고

- [Swift Programming Language - Debugging](https://docs.swift.org/swift-book/LanguageGuide/Debugging.html)
- [NSHipster - Printing Debug Output in Swift](https://nshipster.com/print-debug-output/)

## 더 알아보기

디버그 출력문의 효율적인 사용을 위해 다음과 같은 기능들도 함께 알아보세요!

- 조건부 디버그 출력문(`debugPrintif()`)
- 디버그 출력문 레벨 설정(`#if DEBUG`)