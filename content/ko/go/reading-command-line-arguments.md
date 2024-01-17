---
title:                "컴퓨터 프로그래밍: 명령 줄 인수 읽기"
html_title:           "Go: 컴퓨터 프로그래밍: 명령 줄 인수 읽기"
simple_title:         "컴퓨터 프로그래밍: 명령 줄 인수 읽기"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 무엇이며, 왜? 
명령줄 인수를 읽는 것은 사용자가 프로그램을 실행할 때 추가적인 정보를 입력할 수 있도록 해주는 기능입니다. 프로그래머들은 이를 통해 사용자의 입력에 따라 다양한 기능을 추가할 수 있습니다.

## 방법:
```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    // os.Args를 통해 사용자가 입력한 인수를 읽을 수 있습니다.
    arguments := os.Args
    // 첫 번째 인수는 프로그램의 경로를 의미합니다.
    fmt.Println("프로그램의 경로:", arguments[0])
    // 두 번째 인수부터는 사용자가 추가적으로 입력한 인수들이 출력됩니다.
    fmt.Println("추가적인 인수들:", arguments[1:])
}
```

```
프로그램의 경로: /Users/username/go/bin/myprogram
추가적인 인수들: [arg1 arg2 arg3]
```

## 깊이 파헤치기:
명령줄 인수를 읽는 기능은 예전부터 컴퓨터 과학 분야에서 사용되어 왔습니다. 다른 프로그래밍 언어들에서도 비슷한 기능을 사용할 수 있지만, Go 언어에서는 표준 라이브러리인 ```os```모듈을 통해 쉽게 구현할 수 있습니다.

## 관련 자료:
Go 언어 공식 문서: https://golang.org/pkg/os/#pkg-overview