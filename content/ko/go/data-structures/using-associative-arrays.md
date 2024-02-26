---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:11:02.261329-07:00
description: "\uC5F0\uAD00 \uBC30\uC5F4\uC740 Go\uC5D0\uC11C \uB9F5(map)\uC73C\uB85C\
  \ \uC54C\uB824\uC838 \uC788\uC73C\uBA70, \uAC01\uAC01\uC758 \uACE0\uC720\uD55C \uD0A4\
  \uAC00 \uAC12\uC5D0 \uB9E4\uD551\uB418\uB294 \uD0A4-\uAC12 \uC30D\uC744 \uC800\uC7A5\
  \uD560 \uC218 \uC788\uAC8C \uD574\uC90D\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uB370\uC774\uD130 \uAC80\uC0C9, \uC218\uC815\uC744 \uD6A8\uC728\uC801\
  \uC73C\uB85C \uD558\uACE0, \uACE0\uC720\uD55C \uD0A4\uB97C \uC0AC\uC6A9\uD558\uC5EC\
  \ \uBE60\uB974\uAC8C \uC811\uADFC\uD560 \uC218 \uC788\uB294 \uC694\uC18C\uC758 \uC9D1\
  \uD569\uC744 \uC720\uC9C0\uD558\uAE30 \uC704\uD574 \uB9F5\uC744 \uC0AC\uC6A9\uD569\
  \uB2C8\uB2E4."
lastmod: '2024-02-25T18:49:51.488871-07:00'
model: gpt-4-0125-preview
summary: "\uC5F0\uAD00 \uBC30\uC5F4\uC740 Go\uC5D0\uC11C \uB9F5(map)\uC73C\uB85C \uC54C\
  \uB824\uC838 \uC788\uC73C\uBA70, \uAC01\uAC01\uC758 \uACE0\uC720\uD55C \uD0A4\uAC00\
  \ \uAC12\uC5D0 \uB9E4\uD551\uB418\uB294 \uD0A4-\uAC12 \uC30D\uC744 \uC800\uC7A5\uD560\
  \ \uC218 \uC788\uAC8C \uD574\uC90D\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\
  \uC740 \uB370\uC774\uD130 \uAC80\uC0C9, \uC218\uC815\uC744 \uD6A8\uC728\uC801\uC73C\
  \uB85C \uD558\uACE0, \uACE0\uC720\uD55C \uD0A4\uB97C \uC0AC\uC6A9\uD558\uC5EC \uBE60\
  \uB974\uAC8C \uC811\uADFC\uD560 \uC218 \uC788\uB294 \uC694\uC18C\uC758 \uC9D1\uD569\
  \uC744 \uC720\uC9C0\uD558\uAE30 \uC704\uD574 \uB9F5\uC744 \uC0AC\uC6A9\uD569\uB2C8\
  \uB2E4."
title: "\uC5F0\uAD00 \uBC30\uC5F4 \uC0AC\uC6A9\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?

연관 배열은 Go에서 맵(map)으로 알려져 있으며, 각각의 고유한 키가 값에 매핑되는 키-값 쌍을 저장할 수 있게 해줍니다. 프로그래머들은 데이터 검색, 수정을 효율적으로 하고, 고유한 키를 사용하여 빠르게 접근할 수 있는 요소의 집합을 유지하기 위해 맵을 사용합니다.

## 방법:

Go에서 맵을 생성하고 초기화하는 방법은 여러 가지가 있습니다. 시작하기에 좋은 기본 예시는 다음과 같습니다:

```go
package main

import "fmt"

func main() {
    // 맵 선언 및 초기화
    colors := map[string]string{
        "red":   "#FF0000",
        "green": "#00FF00",
        "blue":  "#0000FF",
    }

    fmt.Println(colors)
    // 출력: map[blue:#0000FF green:#00FF00 red:#FF0000]
}
```

요소를 추가하거나 업데이트하려면, 다음과 같이 키에 값을 할당합니다:

```go
colors["white"] = "#FFFFFF"
fmt.Println(colors)
// 출력: map[blue:#0000FF green:#00FF00 red:#FF0000 white:#FFFFFF]
```

키로 값에 접근하는 것은 간단합니다:

```go
fmt.Println("red의 16진수 코드는:", colors["red"])
// 출력: red의 16진수 코드는: #FF0000
```

요소를 삭제하려면, `delete` 함수를 사용합니다:

```go
delete(colors, "red")
fmt.Println(colors)
// 출력: map[blue:#0000FF green:#00FF00 white:#FFFFFF]
```

맵을 반복하는 것은 for 루프를 사용하여 수행됩니다:

```go
for color, hex := range colors {
    fmt.Printf("키: %s 값: %s\n", color, hex)
}
```

Go의 맵은 정렬되지 않습니다. 반복의 순서는 보장되지 않습니다.

## 심층 분석

Go에서, 맵은 해시 테이블로 구현됩니다. 맵의 각 항목은 키와 값의 두 가지 항목으로 구성됩니다. 항목을 저장하기 위해 키는 해시되며, 적절한 해싱으로 소규모 데이터 집합에 대해 상수 시간 작업을 허용하고, 최악의 경우에는 많은 해시 충돌로 인해 O(n)으로 성능이 저하될 수 있지만 평균 시간 복잡도는 O(1)입니다.

새로운 Go 프로그래머에게 중요한 사실은 맵 타입이 참조 타입이라는 것입니다. 이는 맵을 함수에 전달할 때 그 함수 내에서 맵에 대해 이루어진 변경 사항이 호출자에게 보이게 된다는 의미입니다. 예를 들어, 구조체를 함수에 전달하는 경우와는 달리, 포인터로 전달되지 않는 한 구조체는 복사됩니다.

맵은 연관 배열을 다루는 대부분의 사용 사례에서 매우 다양하고 효율적이지만, 성능이 중요한 애플리케이션에서는 키 분포가 빈번한 충돌을 일으킬 수 있는 경우에 더 예측 가능한 성능 특성을 가진 데이터 구조를 사용하는 것이 유익할 수 있습니다.

고려할 다른 대안은 Go 1.9부터 사용 가능한 `sync.Map`로, 키가 한 번만 쓰여지지만 여러 번 읽히는 사용 사례를 위해 설계되었으며, 이러한 시나리오에서 효율성 향상을 제공합니다. 그러나 일반적인 Go 애플리케이션의 경우, 정규 맵 사용이 관용적이며 단순성과 언어에서의 직접 지원으로 인해 종종 추천되는 접근 방식입니다.
