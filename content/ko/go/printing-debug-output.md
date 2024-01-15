---
title:                "디버그 출력하기"
html_title:           "Go: 디버그 출력하기"
simple_title:         "디버그 출력하기"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## 왜

디버그 출력을 프린트하는 것이 왜 중요한지 궁금하신가요? 간단하게 말해서, 디버그 출력은 프로그램의 오류를 찾아내는 데 매우 유용하며, 코드 작성 과정에서 문제를 파악하는 데 도움이 됩니다.

## 방법

디버그 정보를 출력하는 가장 일반적인 방법은 `fmt.Print()`와 `fmt.Println()` 함수를 사용하는 것입니다. 또 다른 유용한 함수는 `fmt.Printf()`로, 포맷된 문자열을 출력할 수 있습니다.

예를 들어, 다음은 `fmt.Println()` 함수를 사용하여 디버그 정보를 출력하는 코드 예제입니다.

```Go
package main

import "fmt"

func main() {
    name := "John"
    age := 30

    fmt.Println("Name:", name)
    fmt.Println("Age:", age)
}
```
위 코드를 실행하면 다음과 같은 결과가 출력됩니다.

```
Name: John
Age: 30
```

`fmt.Printf()` 함수를 사용하여 특정 형식으로 디버그 정보를 출력할 수도 있습니다. 예를 들어, 아래 코드는 `name`과 `age` 변수를 포맷 문자열 안에 넣어 출력하는 예제입니다.

```Go
package main

import "fmt"

func main() {
    name := "John"
    age := 30

    fmt.Printf("Name: %s\nAge: %d\n", name, age)
}
```
위 코드를 실행하면 다음과 같은 결과가 출력됩니다.

```
Name: John
Age: 30
```

이처럼 `fmt` 패키지의 함수들은 문자열, 정수, 실수 등 다양한 데이터 타입을 지원하고 있어, 디버그 정보를 다양한 형식으로 출력할 수 있습니다.

## 딥 다이브

출력된 디버그 정보가 너무 많거나 복잡할 경우, `log` 패키지를 사용하여 로그 파일로 저장할 수도 있습니다. 예를 들어, 아래 코드는 `log` 패키지를 사용하여 로그 파일에 디버그 정보를 저장하는 예제입니다.

```Go
package main

import (
    "log"
    "os"
)

func main() {
    file, err := os.Open("debug.log")
    if err != nil {
        log.Fatal(err)
    }
    defer file.Close()

    log.SetOutput(file)

    // 디버그 정보를 로그 파일에 저장
    log.Println("Debug info")
}
```

위 코드를 실행하면 `debug.log` 파일에 다음과 같은 내용이 저장됩니다.

```
2019/10/15 14:10:25 Debug info
```

`log` 패키지는 다양한 로깅 기능을 제공하므로, 프로그램의 디버그 정보를 관리하는 데 매우 유용합니다. 더 자세한 정보는 공식 문서를 참조하길 바랍니다.

## 참고자료

- [Go 공식 문서: fmt 패키지](https://golang.org/pkg/fmt/)
- [Go 공식 문서: log 패키지](https://golang.org/pkg/log/)
- [A Tour of Go: Printing](https://tour.golang.org/basics/15)