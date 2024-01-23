---
title:                "표준 오류로 쓰기"
html_title:           "Bash: 표준 오류로 쓰기"
simple_title:         "표준 오류로 쓰기"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
표준 오류는 프로그램이 실행 도중에 발생하는 오류 메시지를 보여주는 출력 통로입니다. 프로그래머는 이를 사용하여 사용자에게 오류 알림을 제공하고, 로그 파일과 오류 메시지를 분리하기 위해 사용합니다.

## How to: (하는 방법)
```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	// 에러 메시지를 표준 오류로 출력
	if _, err := os.Stderr.WriteString("에러 발생!\n"); err != nil {
		fmt.Println("표준 오류로 쓰기 실패:", err)
	}
	
	// fmt.Fprint를 이용해서도 표준 오류 출력 가능
	err := fmt.Fprint(os.Stderr, "또 다른 에러!\n")
	if err != nil {
		fmt.Println("표준 오류로 쓰기 실패:", err)
	}
}
```

Sample Output:
```
에러 발생!
또 다른 에러!
```

## Deep Dive (심층 분석)
표준 오류는 UNIX 시스템에서 시작된 개념으로, 표준 출력(메시지)과 표준 오류(에러)를 분리합니다. Go 언어에서는 `os` 패키지를 통해 `Stderr`을 이용하며, 이는 `*os.File` 타입입니다. 이를 통해 `WriteString`, `Write` 메서드로 쉽게 사용할 수 있습니다. 이외에도 `log` 패키지를 사용하여 오류 로깅을 할 수 있으며, 이는 바로 표준 오류로 출력됩니다.

## See Also (참고 자료)
- [Go by Example: Errors](https://gobyexample.com/errors)
- [Go official documentation on package os](https://golang.org/pkg/os/)
- [Go blog on error handling](https://blog.golang.org/error-handling-and-go)
