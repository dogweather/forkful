---
title:                "컴퓨터 프로그래밍에서의 명령 줄 인수 읽기"
html_title:           "Swift: 컴퓨터 프로그래밍에서의 명령 줄 인수 읽기"
simple_title:         "컴퓨터 프로그래밍에서의 명령 줄 인수 읽기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

왜 나는 매우 자주 읽는 것은 다른 언어와 달리 스위프트에서 커맨드 라인 인수를 읽는 방법에 대해 꽤 복잡하기 때문입니다.

## 어떻게

커맨드 라인 인수를 읽는 가장 간단한 방법은 `CommandLine.arguments` 속성을 사용하는 것입니다. 이 속성은 문자열의 배열로 모든 커맨드 라인 인수를 반환합니다.

예를 들어, 만약 우리가 다음과 같은 코드를 가지고 있다면:

```Swift
let arguments = CommandLine.arguments
print(arguments)
```

우리는 다음과 같은 출력을 볼 수 있을 것입니다:

```
["./myApp", "input.txt", "output.txt"]
```

이 배열의 첫 번째 값은 실행 스크립트의 경로를 보여줍니다. 둘째 및 셋째 값은 우리가 넘겨준 커맨드 라인 인수입니다.

더 나아가, 스위프트는 `CommandLine` 타입에 `argumentCount`라는 속성을 제공합니다. 이 속성은 커맨드 라인 인수의 개수를 나타냅니다. 이를 사용하여 우리는 특정 개수의 인수를 기대하는지 확인할 수 있습니다.

다음은 커맨드 라인 인수를 읽는 구체적인 예제입니다:

```Swift
// 파일 이름이 main.swift인 스크립트에서:

let arguments = CommandLine.arguments
let count = CommandLine.argumentCount

//적어도 2개의 인수가 필요합니다 (실행 스크립트의 경로와 최소 1개의 인수)
if count < 2 {
    print("두 개의 인수가 필요합니다")
    exit(-1)
}

// 두 번째 인수를 통해서 입력 파일 이름을 읽습니다
let inputFile = arguments[1]

// 세 번째 인수를 통해서 출력 파일 이름을 읽습니다
let outputFile = arguments[2]

print("입력 파일: \(inputFile)")
print("출력 파일: \(outputFile)")
```

위의 코드를 실행하면 다음과 같은 출력이 생성됩니다:

```
입력 파일: input.txt
출력 파일: output.txt
```

위의 예제에서 우리는 `CommandLine.argc` 속성을 사용하지 않았습니다. 이 속성은 `CommandLine.argumenCount`와 같은 역할을 하지만 더 짧은 이름을 가지고 있습니다.

## 딥 다이브

앞서 언급했듯이, `CommandLine.arguments`는 문자열의 배열을 반환합니다. 따라서 단순히 `arguments[1]`와 같이 인덱스를 사용하여 특정 인수를 읽을 수 있습니다. 하지만 이는 다소 불안정할 수 있으며, 인수의 순서가 바뀌거나 추가될 경우 제대로 작동하지 않을 수 있습니다.

스위프트에서는 다양한 방법으로 커맨드 라인 인수를 읽을 수 있습니다. 예를 들어 `CommandLine.arguments` 속성의 값을 `CommandLine.parse` 메서드를 사용하여 파싱할 수 있습니다. 이를 사용하면 커맨드 라인 인수를 커스텀 타입으로 변환하고 사용하기 쉬운 객체를 생성할 수 있습니다.

## 더 알아보기

- [Swift Documentation for CommandLine](https://developer.apple.com/documentation/foundation/commandline)
- [Hacking with Swift: Reading command line arguments](https://www.hackingwithswift.com/example-code/system/how-to-read-command-line-arguments-using-commandline)
- [Stack Overflow: How do I pass command line arguments to a Kohen script?](https://stackoverflow.com/questions/25692414/how-do-i-pass-command-line-arguments-to-a-korean-script)