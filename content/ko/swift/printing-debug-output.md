---
title:    "Swift: 디버그 출력 출력"
keywords: ["Swift"]
---

{{< edit_this_page >}}

### 왜
디버그 출력을 프린팅하는 이유는 다음과 같습니다: 우리가 작성한 코드를 이해하고 실제로 어떻게 동작하는지 파악하기 위해서입니다.

### 어떻게
디버그 출력을 프린팅하는 것은 매우 간단합니다. 우선, ```print()``` 함수를 사용하여 출력하려는 내용을 괄호로 둘러싸고 콤마로 구분하여 작성합니다. 다음은 간단한 예시입니다:

```Swift
print("Hello, World!") // Hello, World!
```

또는 변수나 상수의 값을 확인하려면 해당 변수나 상수를 ```print()``` 함수 안에 직접 넣어줄 수도 있습니다:

```Swift
let number = 5
print(number) // 5
```

추가적으로, 조건문이나 반복문을 디버그 출력에 활용할 수도 있습니다. 예를 들어, 아래의 코드를 실행하면 문자열 "even"과 "odd"가 번갈아서 출력됩니다:

```Swift
for number in 1...10 {
    if number % 2 == 0 {
        print("\(number) is even")
    } else {
        print("\(number) is odd")
    }
}
// 1 is odd
// 2 is even
// 3 is odd
// 4 is even
// 5 is odd
// 6 is even
// 7 is odd
// 8 is even
// 9 is odd
// 10 is even
```

### 딥 다이브
디버그 출력을 프린팅하는 것은 처음보다 훨씬 익숙해질 때까지 약간의 연습이 필요할 수 있습니다. 또한, 디버그 출력을 활용하는 유용한 팁도 있습니다. 예를 들어, 변수나 상수의 값에 대한 추가적인 정보를 확인하려면 디버그 출력할 때 해당 변수나 상수를 따옴표로 둘러싸서 문자열로 출력할 수 있습니다:

```Swift
let name = "John"
print("The user's name is \(name).") // The user's name is John.
```

또는 조건문이나 반복문에서 사용되는 변수의 값을 출력하여 코드의 동작을 더욱 분석할 수 있습니다. 또한, ```print()``` 함수에 문자열 이외에도 다양한 데이터 타입의 값들을 넣어주면 해당 값에 대한 정보를 확인할 수 있습니다.

### 참고하기
[Official Swift Documentation on Printing Debug Output](https://docs.swift.org/swift-book/LanguageGuide/PrintingAndDebugging.html) <br>
[Advanced Debugging Tips for Swift](https://www.swiftbysundell.com/articles/advanced-debugging-in-swift/) <br>
[Printing Debug Output to a File in Swift](https://www.programmingwithswift.com/print-debug-output-to-a-file-in-swift/)