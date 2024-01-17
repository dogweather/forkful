---
title:                "표준 오류로 쓰기"
html_title:           "Go: 표준 오류로 쓰기"
simple_title:         "표준 오류로 쓰기"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# 무엇 & 왜?
버그가 발생할 때, 프로그래머들은 보통 프로그램의 오류 메시지를 보고합니다. 이 오류 메시지 중에는 중요한 정보를 포함하는 것들이 있습니다. ‘표준 오류’는 오류 메시지를 출력하기 위한 콘솔 디스플레이입니다. 이것은 프로그래머들이 프로그램 실행 중에 오류를 매우 빠르게 식별하고 추적할 수 있도록 해줍니다.

## 방법:
Go는 standard 라이브러리에서 os 패키지를 제공하여, 표준 오류에 메시지를 출력할 수 있도록 해줍니다. 아래의 예시 코드에서는 os.Stderr 변수를 사용하여 standard error에 메시지를 출력하고, 이를 위해 fmt 패키지의 Fprint 함수를 사용합니다.

```Go
import (
    "fmt"
    "os"
)

func main() {

    fmt.Fprint(os.Stderr, "표준 오류에 메시지 출력하기")
}
```

위의 코드를 실행하면, 표준 오류에 “표준 오류에 메시지 출력하기”라는 메시지가 표시됩니다.

## 딥다이브:
표준 오류를 작성하는 방법은 매우 간단하지만, 오류 메시지의 중요한 부분을 이해하는 것이 중요합니다. 이 기술은 오래된 컴퓨터 시스템에서도 널리 사용되어 왔습니다. 현재의 표준 오류 채널은 제어 콘솔의 디스플레이를 위한 영역이며, 오류 메시지를 적절하게 처리할 수 있도록 합니다.

다른 언어에서는 standard output을 사용하여 간단하게 오류 메시지를 출력합니다. 하지만 Go의 경우 따로 표준 오류 채널을 제공하여, 오류 메시지의 중요성을 강조하고 더 빠르게 작업을 진행할 수 있도록 도와줍니다.

## 관련 자료:
- [Go os 패키지](https://golang.org/pkg/os/)
- [GopherCon 2017: Understanding the OS Package by Bryan Liles](https://www.youtube.com/watch?v=Ut_KKyaVniQ)