---
title:                "컴퓨터 프로그래밍의 제목은 커맨드 라인 인수 읽기입니다."
html_title:           "Go: 컴퓨터 프로그래밍의 제목은 커맨드 라인 인수 읽기입니다."
simple_title:         "컴퓨터 프로그래밍의 제목은 커맨드 라인 인수 읽기입니다."
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

이 기사를 읽는 이유는 사람들이 명령 줄 인수를 읽는 방법을 배우고 싶어하는 경우입니다. 

## 하우 투

명령 줄 인수를 읽는 방법은 Go 프로그래밍 언어에 내장 된 flag 패키지를 사용하는 것입니다. 예를 들어, 다음과 같은 코드를 작성할 수 있습니다:

```Go
// 필요한 패키지를 가져옵니다.
import (
	"flag"
	"fmt"
)

// 명령 줄 인수를 저장할 변수를 선언합니다.
var name string

func main() {
	// flag 패키지를 사용하여 "name"이라는 명령 줄 인수를 정의합니다.
	// 두 번째 인수는 기본값을 지정합니다.
	flag.StringVar(&name, "name", "World", "Enter your name")

	// 명령 줄 인수를 파싱하여 변수에 값을 저장합니다.
	flag.Parse()

	// 인수로 전달된 이름을 출력합니다.
	fmt.Printf("Hello, %s!", name)
}

//출력 결과:
//go run main.go -name=John
//Hello, John!
```

## 딥 다이브

flag 패키지는 명령 줄 인수를 읽는 데 매우 유용합니다. 그러나 여러분은 직접 명령 줄 인수를 읽는 방법을 구현할 수도 있습니다. 그 방법은 os 패키지의 Args 변수를 사용하는 것입니다. Args 변수에는 프로그램의 이름을 제외한 모든 명령 줄 인수가 포함되어 있습니다. 다음과 같은 예제 코드를 참조해보세요:

```Go
import (
	"fmt"
	"os"
)

func main() {
	// os 패키지의 Args 변수를 사용하여 명령 줄 인수를 읽습니다.
	// Args[0]은 프로그램의 이름을 나타냅니다.
	// Args[1:]에는 프로그램의 이름을 제외한 나머지 인수들이 포함됩니다.
	// 이 예제에서는 두 개의 인수를 받고 있습니다.
	for i := 1; i < len(os.Args); i++ {
		fmt.Printf("Arg %d: %s\n", i, os.Args[i])
	}
}

//출력 결과:
//go run main.go Hello World
//Arg 1: Hello
//Arg 2: World
```

## 참고 자료

- [Go Flag 패키지 문서](https://golang.org/pkg/flag/)
- [Go OS 패키지 문서](https://golang.org/pkg/os/)