---
title:                "Swift: 컴퓨터 프로그래밍에서 명령줄 인수 읽기"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

명령줄 인수를 읽는 일은 아주 유용한 기술입니다. 이 블로그 포스트에서는 Swift 프로그래밍 언어로 명령줄 인수를 해석하는 방법을 배우게 될 것입니다.

## 어떻게 하나요?

명령줄 인수를 읽는 방법은 간단합니다. 먼저, ```CommandLine``` 클래스를 import한 다음, ```arguments``` 프로퍼티를 사용하여 사용자가 입력한 인수들을 가져올 수 있습니다.

다음은 간단한 예시 코드입니다:

```Swift
import Foundation

let arguments = CommandLine.arguments
print("사용자가 입력한 인수들: \(arguments)")
```

위 코드를 실행하면 다음과 같은 출력 결과를 얻을 수 있습니다.

```
사용자가 입력한 인수들: ["main", "file1.txt", "file2.txt"]
```

위에서 볼 수 있듯이, ```arguments```는 배열 형태로 입력된 값들을 저장합니다. 배열의 첫 번째 요소는 프로그램의 이름을 나타내는 문자열이며, 그 뒤로는 사용자가 입력한 인수들이 차례대로 나열됩니다.

## 깊이 파고들기

더 복잡한 명령줄 인수를 다루려면, ```CommandLine``` 클래스의 다른 메서드들을 사용할 수 있습니다. 예를 들어, ```CommandLine``` 클래스의 ```option``` 메서드를 사용하면 특정 옵션이 입력되었는 지 확인할 수 있습니다.

아래는 사용자가 입력한 인수 중에서 ```-h``` 또는 ```--help``` 옵션이 입력된 경우에만 도움말 메시지를 출력하는 예제입니다.

```Swift
import Foundation

let arguments = CommandLine.arguments

if arguments.option("h", "help") != nil {
    print("도움말: 이 프로그램은 두 개의 파일을 비교하는 역할을 합니다.")
    print("사용법: main <file1> <file2>")
}
```

위 코드를 통해 명령줄 인수를 더욱 효율적으로 다루고 활용할 수 있게 됩니다.

## See Also

- [Swift documentation for CommandLine class](https://developer.apple.com/documentation/foundation/commandline)
- [How to Read Command Line Arguments in Swift](https://www.hackingwithswift.com/example-code/strings/how-to-read-command-line-arguments-using-commandline)
- [Using Swift as a Scripting Language](https://developer.apple.com/library/archive/documentation/IDEs/Conceptual/swift_debugger_guide/Scripting/Scripting.html)