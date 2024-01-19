---
title:                "명령줄 인수 읽기"
html_title:           "Arduino: 명령줄 인수 읽기"
simple_title:         "명령줄 인수 읽기"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 무엇이고 왜합니까? (What & Why?)

명령 행 인수 읽기는 사용자가 프로그램에 입력 데이터를 제공하게 해주는 강력한 방법입니다. 이것은 프로그래머들이 애플리케이션을 더욱 유연하고 사용자 정의 가능하게 만드는데 도움이 됩니다.

## 어떻게: (How to)

다음은 Go의 `os.Args`를 사용하여 명령 줄 인자를 읽는 간단한 예입니다:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	argsWithProg := os.Args
	argsWithoutProg := os.Args[1:]

	arg := os.Args[3]

	fmt.Println(argsWithProg)
	fmt.Println(argsWithoutProg)
	fmt.Println(arg)
}
```

이 스크립트를 `go run main.go arg1 arg2 arg3`와 같이 실행하면 다음을 출력합니다:

```Go
[main.go arg1 arg2 arg3]
[arg1 arg2 arg3]
arg3
```

## 깊게 파보기 (Deep Dive)

- **역사적 맥락 (Historical Context)**: 초기 프로그래밍 언어는 명령 행 인수를 사용하지 않았습니다. 그러나 시간이 지나면서 이 기능은 더욱 효율적인 프로그램을 만드는 데 도움이 되었습니다.
- **대안 (Alternatives)**: `os.Args` 외에도, `flag` 패키지를 사용하여 명령 줄 인수를 처리할 수 있습니다. 이 패키지는 인수 구문 분석을 제공하여 코드를 더욱 간결하게 만듭니다.
- **구현 세부 사항 (Implementation Details)**: `os.Args`는 슬라이스를 반환합니다. 이 슬라이스의 첫 번째 요소는 프로그램 이름이고, 나머지 요소들은 순서대로 명령 줄 인수입니다.

## 참고하시기 바랍니다 (See Also)

다음은 명령 줄 인수에 대한 추가 정보 및 관련된 토픽에 대한 링크입니다:

- 플래그 패키지에 대한 Go 공식 문서: [https://golang.org/pkg/flag/](https://golang.org/pkg/flag/)
- 명령 줄 인수에 대한 GoByExample: [https://gobyexample.com/command-line-arguments](https://gobyexample.com/command-line-arguments)