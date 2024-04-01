---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:22.162841-07:00
description: "Go \uD504\uB85C\uADF8\uB798\uBC0D\uC5D0\uC11C \uB514\uBC84\uAC70\uB97C\
  \ \uC0AC\uC6A9\uD55C\uB2E4\uB294 \uAC83\uC740 \uC2E4\uD589 \uC911\uC778 \uD504\uB85C\
  \uADF8\uB7A8\uC758 \uC0C1\uD0DC\uB97C \uAC80\uC0AC\uD558\uACE0 \uC218\uC815\uD558\
  \uB294 \uB3C4\uAD6C\uB098 \uAE30\uB2A5\uC744 \uD65C\uC6A9\uD558\uC5EC \uADF8 \uB3D9\
  \uC791\uC744 \uC774\uD574\uD558\uAC70\uB098 \uBB38\uC81C\uB97C \uC9C4\uB2E8\uD558\
  \uB294 \uAC83\uC744 \uB9D0\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB294\
  \ \uD6A8\uC728\uC801\uC73C\uB85C \uBC84\uADF8\uB97C \uCC3E\uACE0 \uC218\uC815\uD558\
  \uACE0, \uC131\uB2A5\uC744 \uCD5C\uC801\uD654\uD558\uBA70, \uCF54\uB4DC\uC758 \uC815\
  \uD655\uC131\uC744 \uBCF4\uC7A5\uD558\uAE30 \uC704\uD574 \uC774\uB97C\u2026"
lastmod: '2024-03-13T22:44:54.464170-06:00'
model: gpt-4-0125-preview
summary: "Go \uD504\uB85C\uADF8\uB798\uBC0D\uC5D0\uC11C \uB514\uBC84\uAC70\uB97C \uC0AC\
  \uC6A9\uD55C\uB2E4\uB294 \uAC83\uC740 \uC2E4\uD589 \uC911\uC778 \uD504\uB85C\uADF8\
  \uB7A8\uC758 \uC0C1\uD0DC\uB97C \uAC80\uC0AC\uD558\uACE0 \uC218\uC815\uD558\uB294\
  \ \uB3C4\uAD6C\uB098 \uAE30\uB2A5\uC744 \uD65C\uC6A9\uD558\uC5EC \uADF8 \uB3D9\uC791\
  \uC744 \uC774\uD574\uD558\uAC70\uB098 \uBB38\uC81C\uB97C \uC9C4\uB2E8\uD558\uB294\
  \ \uAC83\uC744 \uB9D0\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB294 \uD6A8\
  \uC728\uC801\uC73C\uB85C \uBC84\uADF8\uB97C \uCC3E\uACE0 \uC218\uC815\uD558\uACE0\
  , \uC131\uB2A5\uC744 \uCD5C\uC801\uD654\uD558\uBA70, \uCF54\uB4DC\uC758 \uC815\uD655\
  \uC131\uC744 \uBCF4\uC7A5\uD558\uAE30 \uC704\uD574 \uC774\uB97C\u2026"
title: "\uB514\uBC84\uAC70 \uC0AC\uC6A9\uD558\uAE30"
---

## 어떻게:
Go는 `delve`라고 불리는 내장된 디버깅 시설을 제공합니다. 이는 Go 프로그램을 단계별로 실행하고, 프로그램 변수를 검사하며, 표현식을 평가할 수 있게 하는 전 기능을 갖춘 디버깅 도구입니다.

시작하려면, 먼저 `delve`를 설치해야 합니다. 다음을 실행하여 설치할 수 있습니다:

```shell
go get -u github.com/go-delve/delve/cmd/dlv
```

이제 간단한 Go 프로그램을 디버깅해 보겠습니다. `main.go` 프로그램을 고려해 보세요:

```go
package main

import "fmt"

func main() {
    message := "Go에서 디버깅"
    fmt.Println(message)
}
```

이 프로그램을 디버깅하기 시작하려면, 프로젝트 디렉토리에서 터미널을 열고 실행하세요:

```shell
dlv debug
```

이 명령어는 최적화를 비활성화한 상태로 프로그램을 컴파일하고(디버깅 경험을 개선하기 위해), 프로그램을 시작하고 디버거를 붙입니다.

`delve`가 실행되면, 인터랙티브 디버거 쉘에 있게 됩니다. 몇 가지 기본 명령어는 다음과 같습니다:

- `break main.main`은 `main` 함수에 중단점을 설정합니다.
- `continue`는 중단점을 만날 때까지 프로그램 실행을 재개합니다.
- `print message`는 `message` 변수의 값을 출력합니다.
- `next`는 프로그램 실행을 다음 줄로 진행시킵니다.
- `quit`는 디버거를 종료합니다.

변수를 출력하고 중단점에 도달했을 때의 출력은 다음과 같습니다:

```shell
Breakpoint 1 at 0x49ecf3 for main.main() ./main.go:6
> main.main() ./main.go:6 (hits goroutine(1):1 total:1) (PC: 0x49ecf3)
     1: package main
     2:
     3: import "fmt"
     4:
     5: func main() {
     6: =>    message := "Go에서 디버깅"
     7:       fmt.Println(message)
     8: }
(dlv) print message
"Go에서 디버깅"
```

이러한 명령어들을 사용하여, 프로그램을 단계별로 진행하며 상태를 검사하여 프로그램이 어떻게 동작하는지 이해하고 문제점을 식별할 수 있습니다.

## 깊이 있게
`delve`가 GDB(지누 디버거)와 같은 전통적인 도구들보다 Go의 디버깅 도구로서 선호되는 주된 이유는 Go의 실행 모델과 런타임의 특성 때문입니다. GDB는 처음에 Go 런타임을 염두에 두고 설계되지 않았기 때문에, Go 개발자들에게 `delve`가 더 적합한 선택이 됩니다. `Delve`는 Go를 위해 특별히 설계되었으며, Go 루틴, 채널 및 기타 Go 특정 구조에 대한 더 직관적인 디버깅 경험을 제공합니다.

더욱이, `delve`는 Go 프로그램을 사용할 때 기본 GDB가 제공하는 것을 넘어서는 다양한 기능을 지원합니다. 이에는 실행 중인 프로세스에 대한 디버깅 첨부, 조건부 중단점 설정, Go의 동시성 프리미티브를 포함할 수 있는 복잡한 표현식 평가 등이 포함됩니다.

`delve`가 많은 Go 개발자들에게 선택된 디버거이긴 하지만, Go 도구 체인에는 프로파일링을 위한 내장된 `pprof` 도구와 동시성 시각화를 위한 `trace` 도구와 같이 더 가벼운 형태의 디버깅 지원도 포함되어 있다는 점을 주목할 가치가 있습니다. 이러한 도구들은 프로그램 성능 문제나 동시성 버그를 진단하는 데 있어서 때로는 더 빠르거나 더 고차원적인 방법을 제공할 수 있으며, 디버깅 맥락에 따라 보완적이거나 심지어 선호될 수 있습니다.
