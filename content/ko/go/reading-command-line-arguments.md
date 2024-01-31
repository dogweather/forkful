---
title:                "명령줄 인수 읽기"
date:                  2024-01-20T17:56:16.122643-07:00
model:                 gpt-4-1106-preview
simple_title:         "명령줄 인수 읽기"

category:             "Go"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
명령줄 인수 읽기는 사용자가 프로그램 시작 시 제공하는 옵션과 값을 가져옵니다. 이를 통해 사용자가 프로그램의 행동을 동적으로 제어할 수 있습니다.

## How to (방법):
Go에서 명령줄 인수를 다루는 기본적인 방법을 살펴보겠습니다.

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    argsWithProg := os.Args
    argsWithoutProg := os.Args[1:]

    fmt.Println("All Arguments:", argsWithProg)
    fmt.Println("Without Program Name:", argsWithoutProg)
}

// > go run your_program.go arg1 arg2
// All Arguments: [your_program arg1 arg2]
// Without Program Name: [arg1 arg2]
```
위 예제는 프로그램이 모든 인수를 포함하여 실행되는 방법과 프로그램 이름을 제외한 인수들만을 가지는 방법을 보여줍니다.

## Deep Dive (자세히 들여다보기):
OS의 명령줄 인수는 프로그램이 시작될 때 전달됩니다. Go의 `os` 패키지는 이 인수들에 접근하기 위한 `Args` 슬라이스를 제공합니다.

과거에는 `flag`나 `getopt` 같은 별도의 라이브러리를 사용했지만, Go는 자체적인 표준 라이브러리를 통해 이를 지원합니다. `os.Args`는 단순한 슬라이스이며, 첫 번째 요소는 실행 파일의 경로입니다. 실제 인수를 사용하려면 이를 제외한 나머지 슬라이스를 사용해야 합니다.

더 복잡한 인수 처리를 위해서는 `flag` 패키지를 이용합니다. `flag`는 명령줄 인수를 분석하고, 지정된 타입에 따라 값을 자동으로 변환해 줍니다.

## See Also (더 보기):
- Go by Example: Command-Line Arguments: https://gobyexample.com/command-line-arguments
- Go Doc: os package: https://pkg.go.dev/os
- Go Doc: flag package: https://pkg.go.dev/flag
