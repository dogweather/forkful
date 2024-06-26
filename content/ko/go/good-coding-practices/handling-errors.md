---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:58:14.747207-07:00
description: "\uBC29\uBC95: Go\uC5D0\uC11C\uB294 \uC624\uB958 \uCC98\uB9AC\uAC00 \uBA85\
  \uC2DC\uC801\uC73C\uB85C `error` \uD0C0\uC785\uC744 \uC0AC\uC6A9\uD558\uC5EC \uAD00\
  \uB9AC\uB429\uB2C8\uB2E4. \uC2E4\uD328\uD560 \uC218 \uC788\uB294 \uD568\uC218\uB294\
  \ \uB9C8\uC9C0\uB9C9 \uBC18\uD658 \uAC12\uC73C\uB85C \uC624\uB958\uB97C \uBC18\uD658\
  \uD569\uB2C8\uB2E4. \uC774 \uC624\uB958 \uAC12\uC774 `nil`\uC778\uC9C0 \uD655\uC778\
  \uD558\uBA74 \uC624\uB958\uAC00 \uBC1C\uC0DD\uD588\uB294\uC9C0 \uC54C \uC218 \uC788\
  \uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.469202-06:00'
model: gpt-4-0125-preview
summary: "Go\uC5D0\uC11C\uB294 \uC624\uB958 \uCC98\uB9AC\uAC00 \uBA85\uC2DC\uC801\uC73C\
  \uB85C `error` \uD0C0\uC785\uC744 \uC0AC\uC6A9\uD558\uC5EC \uAD00\uB9AC\uB429\uB2C8\
  \uB2E4."
title: "\uC624\uB958 \uCC98\uB9AC\uD558\uAE30"
weight: 16
---

## 방법:
Go에서는 오류 처리가 명시적으로 `error` 타입을 사용하여 관리됩니다. 실패할 수 있는 함수는 마지막 반환 값으로 오류를 반환합니다. 이 오류 값이 `nil`인지 확인하면 오류가 발생했는지 알 수 있습니다.

```go
package main

import (
    "errors"
    "fmt"
)

func Compute(value int) (int, error) {
    if value > 100 {
        return 0, errors.New("value must be 100 or less")
    }
    return value * 2, nil
}

func main() {
    result, err := Compute(150)
    if err != nil {
        fmt.Println("Error:", err)
    } else {
        fmt.Println("Result:", result)
    }
    
    // 오류를 우아하게 처리하기
    anotherResult, anotherErr := Compute(50)
    if anotherErr != nil {
        fmt.Println("Error:", anotherErr)
    } else {
        fmt.Println("Result:", anotherResult)
    }
}
```

위 코드의 샘플 출력:
```
Error: value must be 100 or less
Result: 100
```

이 예제에서, `Compute` 함수는 계산된 값을 반환하거나 오류를 반환합니다. 호출자는 `err`이 `nil`이 아닌지 확인함으로써 오류를 처리합니다.

## 심화
Go의 오류 처리 접근 방식은 명시적인 오류 검사를 요구하는 자명하고 타입 안전한 방식입니다. 이 개념은 자바와 파이썬과 같은 언어에서 볼 수 있는 예외 기반의 오류 처리와 대조됩니다. 여기서 오류는 예외 핸들러에 의해 잡히지 않는 한 호출 스택을 따라 전파됩니다. Go 팀은 명시적인 오류 처리가 오류가 발생한 바로 그 지점에서 프로그래머가 오류를 즉시 다루도록 함으로써 코드를 더 명확하고 신뢰할 수 있게 만든다고 주장합니다.

그러나 일부 비판은 이 패턴이 오류가 발생하기 쉬운 작업이 많은 복잡한 함수에서 특히 장황한 코드로 이어질 수 있다고 언급합니다. 이에 대응하여, Go의 새로운 버전들은 오류 래핑 같은 더 정교한 오류 처리 기능을 도입하여 원래의 오류 정보를 잃지 않으면서 오류에 대한 맥락을 제공하기 쉽게 만들었습니다. 커뮤니티는 check/handle 같은 새로운 오류 처리 메커니즘에 대한 제안도 보았지만, 이들은 제 마지막 업데이트 시점까지 논의 중에 있습니다.

Go의 오류 처리 철학은 프로그램의 정상적인 흐름의 일부로 오류를 이해하고 계획하는 것을 강조합니다. 이 접근 방식은 보다 탄력적이고 예측 가능한 소프트웨어 개발을 장려하지만, 보일러플레이트 코드의 잠재적 증가를 가져올 수 있습니다. 특히 복잡한 경우를 위한 오류 처리를 간소화하는 대안적 패턴과 라이브러리가 존재하지만, Go의 내장된 `error` 타입은 언어에서 오류 처리의 기반을 유지합니다.
