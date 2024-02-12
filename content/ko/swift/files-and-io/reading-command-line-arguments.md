---
title:                "명령줄 인수 읽기"
aliases:
- /ko/swift/reading-command-line-arguments/
date:                  2024-01-20T17:57:00.031858-07:00
model:                 gpt-4-1106-preview
simple_title:         "명령줄 인수 읽기"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가요? 왜 사용하나요?)
커맨드 라인 인자를 읽는 것은 사용자가 프로그램 실행 시 제공하는 입력 값입니다. 프로그래머들은 다양한 시나리오에서 사용자 맞춤형 동작을 구현하기 위해 이를 사용합니다.

## How to: (어떻게 사용하나요?)
Swift 프로그램에서 커맨드 라인 인자를 읽으려면 `CommandLine`을 사용합니다. 간단한 예제를 살펴보죠.

```Swift
// main.swift
for argument in CommandLine.arguments {
    print("Argument: \(argument)")
}
```

터미널에서 이렇게 실행하면 됩니다:
```shell
$ swift run MyProgram arg1 arg2
```

출력 예시:
```
Argument: /path/to/MyProgram
Argument: arg1
Argument: arg2
```

`CommandLine.arguments`는 배열이며, 첫 번째 아이템은 실행 파일의 경로입니다.

## Deep Dive (심화 정보)
커맨드 라인 인자를 읽는 과정은 유닉스와 같은 시스템에서 시작되었고, 매우 오래된 전통입니다. 대안으로 환경 변수(read with `ProcessInfo.processInfo.environment`) 또는 구성 파일을 사용할 수도 있습니다.

실제 구현을 보면, Swift의 `CommandLine`은 C 언어의 `argc`와 `argv`에 기반을 두고 있습니다. `CommandLine.arguments`는 `argv`를 Swift 배열로 변환한 것입니다. 첫 번째 인자가 실행 파일인 것은 `argv`의 전통을 따르는 것입니다.

## See Also (더 알아보기)
- [Swift Documentation - CommandLine](https://developer.apple.com/documentation/swift/commandline)
- [GNU - Using the Command Line](https://www.gnu.org/software/libc/manual/html_node/Argument-Syntax.html)
