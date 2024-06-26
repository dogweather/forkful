---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:06:46.658561-07:00
description: "\uBC29\uBC95: Go\uB294 `os` \uD328\uD0A4\uC9C0\uB97C \uD1B5\uD574 \uBA85\
  \uB839\uC904 \uC778\uC218\uC5D0 \uC9C1\uC811 \uC811\uADFC\uC744 \uC81C\uACF5\uD558\
  \uBA70, \uD2B9\uD788 \uBB38\uC790\uC5F4 \uBC30\uC5F4\uC778 `os.Args`\uB97C \uC0AC\
  \uC6A9\uD569\uB2C8\uB2E4. \uC2DC\uC791\uD558\uAE30 \uC704\uD55C \uAC04\uB2E8\uD55C\
  \ \uC608\uC81C\uB97C \uC0B4\uD3B4\uBCF4\uACA0\uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.482814-06:00'
model: gpt-4-0125-preview
summary: "Go\uB294 `os` \uD328\uD0A4\uC9C0\uB97C \uD1B5\uD574 \uBA85\uB839\uC904 \uC778\
  \uC218\uC5D0 \uC9C1\uC811 \uC811\uADFC\uC744 \uC81C\uACF5\uD558\uBA70, \uD2B9\uD788\
  \ \uBB38\uC790\uC5F4 \uBC30\uC5F4\uC778 `os.Args`\uB97C \uC0AC\uC6A9\uD569\uB2C8\
  \uB2E4."
title: "\uBA85\uB839 \uC904 \uC778\uC218 \uC77D\uAE30"
weight: 23
---

## 방법:
Go는 `os` 패키지를 통해 명령줄 인수에 직접 접근을 제공하며, 특히 문자열 배열인 `os.Args`를 사용합니다. 시작하기 위한 간단한 예제를 살펴보겠습니다:

```go
package main

import (
    "fmt"
    "os"
)

func main() {
    // os.Args는 명령줄 인수에 대한 접근을 제공
    fmt.Println("명령줄 인수:", os.Args)

    if len(os.Args) > 1 {
        // 첫 번째 인수(프로그램 이름)는 건너뛰고 인수를 순회
        for i, arg := range os.Args[1:] {
            fmt.Printf("인수 %d: %s\n", i+1, arg)
        }
    } else {
        fmt.Println("명령줄 인수가 제공되지 않았습니다.")
    }
}
```

`go run yourprogram.go arg1 arg2`로 실행했을 때의 샘플 출력은 다음과 같을 수 있습니다:

```
명령줄 인수: [/tmp/go-build123456789/b001/exe/yourprogram arg1 arg2]
인수 1: arg1
인수 2: arg2
```

이는 프로그램 이름을 포함한 모든 인수를 출력한 다음, 제공된 각 인수를 순회하며 출력합니다. 더 제어된 인수 분석이 필요한 경우, 명령줄 옵션을 분석하기 위해 `flag` 패키지를 고려할 수 있습니다.

## 심층 탐구
역사적으로, 명령줄 인수에 접근하는 것은 C 프로그래밍만큼 오래된 관행이며, 여기서 `argc`와 `argv[]`가 유사한 목적을 제공합니다. Go에서는 `os.Args`가 직관적이지만 고의적으로 기본적인 방식을 사용합니다. 복잡한 시나리오, 예를 들어 플래그나 옵션을 처리하는 경우, Go는 로버스트한 파싱 기능을 제공하는 `flag` 패키지를 제공합니다. 이는 위치 기반 인수만 이상이 필요한 애플리케이션의 경우 "더 나은" 대안으로 간주될 수 있습니다.

Go의 접근법이 명령줄 인수를 연관 배열 또는 객체로 내장 파싱하는 일부 스크립팅 언어와 달리, 프로그래머들이 기본적인 요구 사항을 위해 수동으로 `os.Args`를 사용하여 파싱을 처리하거나 더 진보된 시나리오에 대해서는 `flag` 패키지를 활용해야 합니다. 이 설계는 Go의 핵심 언어를 간단하게 유지하면서 일반적인 작업을 위한 강력한 표준 라이브러리를 제공하는 Go의 철학을 반영합니다. 이는 내장 파싱에 익숙한 사람들에게는 약간의 학습 곡선을 도입할 수 있지만, 더 큰 유연성을 제공하고 명령줄 인수 처리에 대한 더 깊은 이해를 장려합니다.
