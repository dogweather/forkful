---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:58:39.603739-07:00
description: "\uBB38\uC790\uC5F4 \uBCF4\uAC04(string interpolation)\uC740 \uBCC0\uC218\
  \uB97C \uD3EC\uD568\uD558\uC5EC \uBB38\uC790\uC5F4\uC744 \uAD6C\uC131\uD558\uB294\
  \ \uBC29\uBC95\uC73C\uB85C, \uB3D9\uC801 \uBB38\uC790\uC5F4 \uC0DD\uC131\uC744 \uAC00\
  \uB2A5\uD558\uAC8C \uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740\
  \ \uC774\uB97C \uC0AC\uC6A9\uD558\uC5EC \uBA54\uC2DC\uC9C0\uB97C \uB9DE\uCDA4 \uC124\
  \uC815\uD558\uAC70\uB098, URL\uC744 \uAD6C\uC131\uD558\uAC70\uB098, SQL \uCFFC\uB9AC\
  \uB97C \uC0DD\uC131\uD558\uB294 \uB4F1\uC758 \uC791\uC5C5\uC744 \uC218\uD589\uD558\
  \uBA70, \uC774\uB97C \uD1B5\uD574 \uCF54\uB4DC\uC758\u2026"
lastmod: '2024-03-13T22:44:54.431759-06:00'
model: gpt-4-0125-preview
summary: "\uBB38\uC790\uC5F4 \uBCF4\uAC04(string interpolation)\uC740 \uBCC0\uC218\
  \uB97C \uD3EC\uD568\uD558\uC5EC \uBB38\uC790\uC5F4\uC744 \uAD6C\uC131\uD558\uB294\
  \ \uBC29\uBC95\uC73C\uB85C, \uB3D9\uC801 \uBB38\uC790\uC5F4 \uC0DD\uC131\uC744 \uAC00\
  \uB2A5\uD558\uAC8C \uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uBCF4\uAC04\uD558\uAE30"
weight: 8
---

## 방법:
Go에서는 주로 `fmt` 패키지를 사용하여 문자열 보간을 달성합니다. 특히 `Sprintf` 함수를 사용하면, 포맷 지정 동사를 지정하여 변수를 문자열에 주입할 수 있습니다. 이 동사들은 포맷 문자열에서 자리 표시자 역할을 하며 주어진 변수의 값으로 대체됩니다. 사용 방법은 다음과 같습니다:

```go
package main

import (
    "fmt"
)

func main() {
    name := "Jane"
    age := 28

    // Sprintf를 사용한 문자열 보간
    message := fmt.Sprintf("Hello, my name is %s and I am %d years old.", name, age)
    fmt.Println(message) // 출력: Hello, my name is Jane and I am 28 years old.
}
```

`%s`는 문자열에, `%d`는 정수에 사용됩니다. `fmt` 패키지 문서에서 다른 데이터 유형에 대한 포맷 지정 동사의 포괄적인 목록을 제공합니다.

## 심층 탐구
문자열 보간의 개념은 많은 프로그래밍 언어에서 존재하지만, 구문과 기능 면에서 차이가 있습니다. Go에서는 `fmt` 패키지의 `Sprintf` 함수가 가장 일반적으로 사용되는 접근 방식이지만, 단순한 결합이 필요한 경우나 성능에 매우 민감한 코드에서 작업할 때 항상 가장 효율적인 방법은 아닙니다.

`fmt` 패키지는 런타임에 변수의 타입을 동적으로 해석하기 위해 리플렉션(reflection)을 사용합니다. 이는 유연하지만 오버헤드를 발생시킵니다. 성능이 중요한 시나리오에서는 직접 문자열 결합이나 `strings.Builder` 유형이 더 나은 대안을 제공할 수 있습니다. 직접 결합은 직관적이지만 변수가 많을 때 다루기 어려워질 수 있습니다. 반면 `strings.Builder`는 루프 내에서 복잡한 문자열을 구성하거나 많은 변수를 다루는 경우 더 성능적이고 가독성이 높은 방법을 제공합니다:

```go
var sb strings.Builder
sb.WriteString("Hello, my name is ")
sb.WriteString(name)
sb.WriteString(" and I am ")
sb.WriteString(strconv.Itoa(age))
sb.WriteString(" years old.")
message := sb.String()

fmt.Println(message) // 이전과 동일한 출력
```

결국, `fmt.Sprintf`, 직접 결합, 그리고 `strings.Builder` 사이의 선택은 애플리케이션의 특정 요구 사항, 예를 들어 구성되는 문자열의 복잡성 및 성능 고려 사항에 따라 달라집니다.
