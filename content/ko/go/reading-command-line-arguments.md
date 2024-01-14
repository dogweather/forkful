---
title:                "Go: 컴퓨터 프로그래밍을 위한 명령 줄 인수 읽기"
simple_title:         "컴퓨터 프로그래밍을 위한 명령 줄 인수 읽기"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

Go 프로그래밍을 할 때, 명령줄 인수를 읽는 방법은 중요한 요소입니다. 명령줄 인수를 읽는 것은 프로그램의 실행 중에 사용자에게 입력을 받는다는 것을 의미합니다. 이것이 왜 필요한지 짧게 알아보겠습니다.

## 어떻게

Go에서 우리는 `os.Args`를 사용하여 명령줄 인수를 읽을 수 있습니다. 예를 들어, "hello.go"라는 파일을 실행할 때, 다음과 같은 명령줄 인수를 전달하면:

```
go run hello.go first second third
```

이렇게 코드를 작성합니다:

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    args := os.Args[1:]
    fmt.Println(args)
}
```

위의 코드를 실행하면, 다음과 같은 출력이 나옵니다:

```
[first second third]
```

위의 예제에서 `os.Args` 배열의 첫 번째 요소는 프로그램의 이름을 포함하지 않기 때문에 `args := os.Args[1:]`를 사용하여 프로그램의 이름을 제외하고 나머지 인수를 가져올 수 있습니다. 그리고 `fmt.Println()`을 사용하여 인수를 출력합니다.

## 딥 다이브

Go에서 명령줄 인수를 읽는 또 다른 방법은 `flag` 패키지를 사용하는 것입니다. 이 패키지는 훨씬 더 정교하게 명령줄 인수를 처리할 수 있습니다. 다양한 옵션 값을 설정하고 플래그를 통해 프로그램의 동작을 제어하는 것이 가능합니다. 코드를 보자면:

```Go
package main

import (
    "flag"
    "fmt"
)

var name = flag.String("name", "defaultName", "Enter your name")

func main() {
    flag.Parse()
    fmt.Println("Hello, " + *name)
}
```

이 코드를 실행하면 `go run hello.go -name=John`을 통해 출력 결과로 "Hello, John"이 나옵니다.

## 참고

- [Go 문서: Command-line arguments](https://golang.org/doc/articles/wiki/#command-line-arguments)
- [Go 패키지: flag](https://golang.org/pkg/flag/)
- [타르탈렛리's 블로그: Go's flag package](https://www.calhoun.io/how-to-access-command-line-flags-in-go/)
- [Why Go is better than cmd prompt](https://blog.logrocket.com/why-go-is-better-than-command-prompt-for-writing-cli-tools/)