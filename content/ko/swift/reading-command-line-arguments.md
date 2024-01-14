---
title:    "Swift: 컴퓨터 프로그래밍에서 명령 줄 인수 읽기"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

커맨드 라인 매개변수를 읽는 것에 참여하는 이유는 커맨드 라인에서 입력된 정보를 활용하여 프로그램의 동작을 조정할 수 있기 때문입니다.

## 방법

커맨드 라인 매개변수를 읽는 방법은 간단합니다. 먼저 `CommandLine.arguments` 속성을 사용하여 매개변수들을 배열로 가져올 수 있습니다. 그리고 각 매개변수는 문자열로 표현되기 때문에 원하는 형식으로 변환할 수 있습니다. 예를 들어, 매개변수로 전달된 숫자를 정수형으로 변환하려면 `Int()` 로 감싸주면 됩니다.

```Swift
// 커맨드 라인 매개변수를 배열로 가져오기
let arguments = CommandLine.arguments

// 첫 번째 매개변수 출력
print("첫 번째 매개변수: \(arguments[0])")

// 두 번째 매개변수를 정수형으로 변환하여 출력
if arguments.count > 1 {
    print("두 번째 매개변수: \(Int(arguments[1]) ?? 0)")
}
```

커맨드 라인에서 `swift run` 명령어로 위의 코드를 실행하면 다음과 같은 결과를 얻을 수 있습니다.

```
$ swift run ArgumentsDemoHello
첫 번째 매개변수: ArgumentsDemoHello
두 번째 매개변수: 0
```

## 깊이 파고들기

커맨드 라인 매개변수를 읽는 작업은 프로그램을 만들 때 매우 유용합니다. 예를 들어, 프로그램의 특정 옵션을 설정하거나 특정 동작을 수행할 때 커맨드 라인 매개변수를 사용하여 사용자가 직접 프로그램을 제어할 수 있게 할 수 있습니다. 또한, 커맨드 라인 매개변수를 통해 사용자 입력을 받는 대화형 프로그램을 만들 수도 있습니다.

## 더 찾아보기

이번 포스트에서는 기본적인 커맨드 라인 매개변수 읽는 방법에 대해 알아보았습니다. 더 자세한 정보를 원한다면 아래의 링크를 참고해보세요.

- [Swift Command Line Tutorial](https://www.raywenderlich.com/163857/command-line-programs-macos-tutorial)
- [Parsing Command-Line Arguments in Swift](https://www.swiftbysundell.com/articles/parsing-command-line-arguments-in-swift/)
- [Command-Line Arguments](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID333)