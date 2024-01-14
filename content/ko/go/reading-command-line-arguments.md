---
title:    "Go: 컴퓨터 프로그래밍에서의 명령 줄 인자 읽기"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜
왜 *command line arguments*를 읽는 데에 집중하는 것이 좋을까요? <Organization> 프로그래머로서, 많은 작업을 효율적으로 처리하기 위해서는 사용자의 입력을 이해하고 활용하는 것이 중요합니다. 이 포스트에서는 *Go* 언어에서 *command line arguments*를 어떻게 읽을 수 있는지 알아보겠습니다.

## 방법
다음은 *Go* 코드를 사용하여 *command line arguments*를 읽는 방법입니다. 

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    // os.Args를 이용하여 커맨드라인 입력을 저장합니다.
    args := os.Args
    
    // 첫번째 요소는 프로그램명이므로 무시합니다.
    // 실제 입력은 두번째 요소부터 시작합니다.
    // 반복문을 통해 모든 인자를 출력합니다.
    for i := 1; i < len(args); i++ {
        fmt.Println(args[i])
    }
}
```

이 예시 코드를 *main.go*라는 이름으로 저장하고, 커맨드 라인에서 `go run main.go argument1 argument2`와 같이 실행하면 아래와 같은 출력을 볼 수 있습니다.

```
argument1
argument2
```

## 딥 다이브
위의 예시 코드에서 `os.Args`는 입력된 모든 커맨드라인 인자를 슬라이스로 저장합니다. 여기서 슬라이스는 배열과 유사한 형태로, 인덱스를 통해 각 인자에 접근할 수 있습니다. 또한, `os.Args[0]`는 프로그램명을 나타내며 실제 커맨드 라인 인자는 `os.Args[1]`부터 시작합니다.

## 이것도 봐주세요
- [Go 공식 문서 - Command-line arguments](https://golang.org/doc/articles/Command-line-arguments.md)
- [Go 언어 슬라이스(Slices) 개념 이해하기](https://velog.io/@kimmachinegun/Golang-%EC%8A%AC%EB%9D%BC%EC%9D%B4%EC%8A%A4-Slices)