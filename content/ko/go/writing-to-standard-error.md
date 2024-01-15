---
title:                "표준 에러에 쓰는 방법"
html_title:           "Go: 표준 에러에 쓰는 방법"
simple_title:         "표준 에러에 쓰는 방법"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜

누군가 표준 오류 출력에 쓰는 것이 왜 중요한지 궁금하다면 이런 이유가 있습니다: 프로그램에서 예외 상황이 발생할 경우 오류 메시지를 표시하기 위함입니다.

## 방법

```Go
import "fmt"

func main() {
  fmt.Fprintf(os.Stderr, "에러 발생: %s", err)
}
```

위의 예시 코드는 프로그램에서 예외 상황이 발생할 때 `os.Stderr`를 통해 오류 메시지를 출력하는 방법을 보여줍니다. 이렇게 함으로써 사용자에게 알리고 트러블슈팅을 도와줄 수 있습니다.

### 추가 팁

표준 오류 출력은 `os` 패키지의 `fmt.Fprintf()` 함수를 사용하여 작성할 수 있습니다. 또한 `os.Stderr` 대신 `os.Stdout`를 사용하면 일반적인 로그 메시지를 출력할 수 있습니다.

## 깊이 파고들기

프로그램에서 오류 메시지를 표시하는 다양한 방법이 있지만, 표준 오류 출력을 사용하는 것은 가장 간단하고 효과적인 방법입니다. 다른 출력 메서드는 오류를 무시하거나 중요하지 않은 로그 메시지로 처리하기 때문에, 예외 상황을 파악하는 데 도움을 주지 못할 수 있습니다.

## 참고 자료

- [Golang 공식 문서 - fmt 패키지](https://golang.org/pkg/fmt/)
- [Golang 공식 문서 - os 패키지](https://golang.org/pkg/os/)
- [Go 프로그래밍 언어 기초 강좌](https://youtu.be/U1VH8aCKF3U)
- [Go 언어 튜토리얼](https://www.tutorialspoint.com/go/index.htm)

## 더 보기