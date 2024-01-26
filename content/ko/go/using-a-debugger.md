---
title:                "디버거 사용하기"
date:                  2024-01-26T03:50:02.130294-07:00
model:                 gpt-4-0125-preview
simple_title:         "디버거 사용하기"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/using-a-debugger.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
디버거를 사용하는 것은 코드의 정글에서 GPS를 가지고 있는 것과 같습니다; 문제의 원인으로 안내해 줍니다. 프로그래머들은 자신들의 코드를 단계별로 진행하면서, 변수를 검사하고 흐름을 이해함으로써 버그를 잡고 성능을 최적화하기 위해 디버거를 사용합니다.

## 사용 방법:
Go에는 `dlv`라는 내장 도구가 있습니다. 시작하려면, Delve를 설치하고, 간단한 Go 프로그램을 작성한 다음 디버거를 통해 실행합니다.

```Go
// 먼저, Delve 설치
// go get -u github.com/go-delve/delve/cmd/dlv

// 예제 Go 프로그램, main.go로 저장
package main

import "fmt"

func main() {
    message := "Delve로 디버깅하기!"
    fmt.Println(message)
}

// Delve로 프로그램 실행
// dlv debug

// 몇 가지 기본 Delve 명령어:
// (dlv) break main.main // 함수 main에 중단점을 설정
// (dlv) continue // 중단점이나 프로그램 종료까지 실행
// (dlv) step // 프로그램을 한 단계씩 실행
// (dlv) print message // 변수 'message'의 현재 값을 출력
// (dlv) quit // Delve 종료
```

`dlv debug`를 실행하면 디버깅 세션이 시작됩니다. 설정한 중단점에 도달하면 프로그램을 단계별로 진행하며 내부적으로 무슨 일이 일어나고 있는지 확인할 수 있습니다.

## 심층 탐구
역사적으로, Go 프로그래머들은 GDB(GNU Debugger) 같은 여러 도구를 디버깅에 사용했지만, GDB가 Go의 런타임과 고루틴에 맞춰져 있지 않아 어려움을 겪었습니다. Delve는 Go의 독특한 기능에 더 나은 지원을 제공하며 구원투수 역할을 했습니다.

`go-dbg`와 같은 Delve의 대안이 있으며, Visual Studio Code 및 GoLand와 같은 IDE 내에 통합된 디버거 지원도 있는데, 이는 사용자 친화적인 경험을 제공하기 위해 Delve를 둘러싼 형태입니다.

구현 측면에서, Delve는 `runtime` 및 `debug/gosym` 패키지 등 다른 패키지를 사용하여 Go 프로그램 기호와 런타임 정보에 접근하고 해석합니다. 새로운 언어 기능 및 버전에 맞춰 지속적으로 업데이트됩니다.

## 참조
- Delve의 공식 저장소: https://github.com/go-delve/delve
- Go 팀의 Go 디버거 튜토리얼: https://golang.org/doc/gdb
- Visual Studio Code Go 디버깅: https://code.visualstudio.com/docs/languages/go#_debugging