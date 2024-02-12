---
title:                "문자열 보간하기"
aliases:
- /ko/go/interpolating-a-string/
date:                  2024-02-03T17:58:39.603739-07:00
model:                 gpt-4-0125-preview
simple_title:         "문자열 보간하기"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/interpolating-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇이며 왜?

문자열 보간(string interpolation)은 변수를 포함하여 문자열을 구성하는 방법으로, 동적 문자열 생성을 가능하게 합니다. 프로그래머들은 이를 사용하여 메시지를 맞춤 설정하거나, URL을 구성하거나, SQL 쿼리를 생성하는 등의 작업을 수행하며, 이를 통해 코드의 가독성과 유지보수성을 높일 수 있습니다.

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
