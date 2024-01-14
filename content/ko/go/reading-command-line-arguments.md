---
title:    "Go: 컴퓨터 프로그래밍에서 명령 줄 인수 읽기"
keywords: ["Go"]
---

{{< edit_this_page >}}

## 왜

Go 프로그래밍을 하다 보면 종종 커맨드 라인 인자를 읽어와야 할 때가 있습니다. 이는 프로그램을 더 유연하고 사용자 정의할 수 있게 만들어줍니다. 따라서 커맨드 라인 인자를 읽는 방법을 알아두는 것은 매우 중요합니다.

## 어떻게

커맨드 라인에서 인자를 읽는 것은 간단한 작업입니다. 우선 `os` 패키지를 import 해야 합니다. 그리고 `os.Args` 변수를 통해 인자를 읽을 수 있습니다.

```Go
import "os"

func main() {
    // 커맨드 라인 인자를 출력합니다.
    fmt.Println(os.Args)

    // 첫 번째 인자는 프로그램의 이름입니다.
    // 두 번째 이후의 인자는 프로그램의 인자입니다.
    // 예를 들어 "go run main.go hello world"를 실행하면
    // os.Args는 [go run main.go hello world]가 됩니다.
}
```

출력 결과는 다음과 같습니다.

```
[go run main.go hello world]
```

인자를 원하는대로 사용하면 됩니다. 예를 들어 위의 예시에서 `hello`와 `world`를 인자로 사용할 수 있습니다.

## 딥 다이브

`os.Args`는 슬라이스 타입입니다. 슬라이스는 여러 값을 담을 수 있는 데이터 타입으로, 배열과 유사합니다. 슬라이스는 `[]` 기호를 사용하여 선언할 수 있고, `len()` 함수를 통해 길이를 얻을 수 있습니다. 모든 인덱스는 0부터 시작하며, 예를 들어 `os.Args[0]`은 프로그램의 이름을 나타냅니다.

커맨드 라인에서 전달한 인자를 원하는 형태로 사용하려면 문자열을 다른 타입으로 변환할 필요가 있을 수 있습니다. 예를 들어 정수로 변환하기 위해서는 `strconv` 패키지를 사용해야 합니다.

```Go
import (
    "os"
    "fmt"
    "strconv"
)

func main() {
    // 두 번째 인자를 정수로 변환합니다.
    num, err := strconv.Atoi(os.Args[1])
    if err != nil {
        fmt.Println("잘못된 입력입니다.")
        return
    }

    // 입력한 값이 1부터 3 사이인지 확인합니다.
    if num >= 1 && num <= 3 {
        fmt.Println("사용자가 입력한 값:", num)
    } else {
        fmt.Println("입력한 값은 1에서 3 사이여야 합니다.")
    }
}
```

커맨드 라인에서 `2`를 입력하면 다음과 같은 출력이 나타납니다.

```
사용자가 입력한 값: 2
```

하지만 `4`를 입력하면 다음과 같은 출력이 나타납니다.

```
입력한 값은 1에서 3 사이여야 합니다.
```

## 더 알아보기

커맨드 라인 인자를 사용하는 방법에 대해 좀 더 알아보려면 다음 링크를 참고해보세요.

- [Go 공식 문서 - 커맨드 라인 인자 읽기](https://golang.org/pkg/os/#pkg-variables)
- [Effective Go - 커맨드 라인 인자](https://golang.org/doc/effective_go.html#command-line-arguments)

## 또 다른 링크

- [Go 언어 살펴보기](https://golang.org/doc/)