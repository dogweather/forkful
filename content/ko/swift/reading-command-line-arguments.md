---
title:    "Swift: 컴퓨터 프로그래밍에서 명령 줄 인수 읽기"
keywords: ["Swift"]
---

{{< edit_this_page >}}

# 왜
일반적으로, 명령 줄 인수는 프로그래밍에서 매우 유용하게 사용됩니다. 예를 들어, 사용자가 프로그램 실행 시 입력한 추가 정보를 읽을 수 있고, 다양한 매개변수를 설정할 수 있습니다. 따라서 명령 줄 인수를 읽는 방법은 프로그래밍 기본 기술 중 하나로 간주됩니다. 이 블로그 포스트는 명령 줄 인수를 읽는 방법에 대해 간단한 예제와 함께 소개할 것입니다.

## 어떻게
우선, ```CommandLine``` 클래스를 import 해야 합니다. 그리고 ```arguments``` 프로퍼티를 사용해서 인수를 읽을 수 있습니다. 예를 들어, ```swift CommandLine.arguments``` 식으로 사용하면 입력한 인수들의 배열을 리턴합니다. 아래는 명령 줄에서 숫자를 받아서 출력하는 예제 코드입니다.

```swift
let arguments = CommandLine.arguments
if arguments.count > 1 {
    let inputNumber = arguments[1]
    print("입력한 숫자는 \(inputNumber)입니다!")
} else {
    print("숫자를 입력해 주세요.")
}

```
위 코드를 실행하고 ```swift test.swift 5```와 같이 인수를 전달하면, 출력은 ```입력한 숫자는 5입니다!```가 됩니다.

## 깊게 파고들기
더 많은 깊은 내용을 알고 싶다면, 명령줄을 읽는 데 사용되는 C 언어의 ```argc```와 ```argv```의 개념을 알아볼 필요가 있습니다. ```CommandLine``` 클래스는 내부적으로 C 명령줄 인수를 다루기 때문입니다. 또한, 명령줄 인수를 입력할 때의 유의점들에 대해서도 공부할 수 있습니다. 예를 들어, 인수로 문자열보다는 정수를 입력하는 게 더 효율적일 수 있습니다.

## 자세히 보기

- [Swift 문서: Command-Line 인수 읽기](https://docs.swift.org/swift-book/LanguageGuide/ControlFlow.html#ID525)
- [XCode Playground에서 명령줄 인수 읽기](https://www.hackingwithswift.com/example-code/system/how-to-read-command-line-arguments-using-commandline)  
- [C 언어에서 명령줄 인수 다루기](https://www.tutorialspoint.com/cprogramming/c_command_line_arguments.htm)

# 더 알아보기

- [Swift 프로그래밍 코너 블로그](https://swiftcafe.io/tags/CommandLineArgument)에서 다양한 명령줄 인수 읽기 예제를 살펴보세요. 
- [Apple Developer Forum](https://developer.apple.com/forums/tags/command%20line)에서 명령줄 인수와 관련된 질문과 답변을 읽어보세요.