---
title:                "숫자 반올림하기"
date:                  2024-01-26T03:44:54.757753-07:00
model:                 gpt-4-0125-preview
simple_title:         "숫자 반올림하기"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/rounding-numbers.md"
---

{{< edit_this_page >}}

## 무엇인가 & 왜인가?
숫자를 반올림하는 것은 숫자를 가장 가까운 전체수나 지정된 소수점 자리로 조정하는 것을 의미합니다. 이는 값을 단순화하거나, 더 읽기 쉽게 만들거나, 통화와 같은 특정 제약조건에 맞게 조정할 때 수행됩니다.

## 어떻게 하나:
Go의 `math` 패키지가 반올림을 위한 친구입니다. 단순함을 위해 `math.Round`, `math.Floor`, 그리고 `math.Ceil`을 사용하세요:

```go
package main

import (
	"fmt"
	"math"
)

func main() {
	number := 3.14159
	fmt.Println("Round:", math.Round(number))  // 가장 가까운 전체 수로 반올림
	fmt.Println("Floor:", math.Floor(number)) // 내림
	fmt.Println("Ceil: ", math.Ceil(number))  // 올림
}
```

샘플 출력:
```
Round: 3
Floor: 3
Ceil: 4
```

특정 소수점 자리에 대해서는, 곱한 후 반올림하고 나눕니다:

```go
func roundToDecimalPlace(number float64, decimalPlaces int) float64 {
	shift := math.Pow(10, float64(decimalPlaces))
	return math.Round(number*shift) / shift
}

func main() {
	number := 3.14159
	fmt.Println("2 소수점 자리로 반올림:", roundToDecimalPlace(number, 2))
}
```

샘플 출력:
```
2 소수점 자리로 반올림: 3.14
```

## 깊이 들여다보기
숫자를 반올림하는 것은 새로운 것이 아니며—이는 고대 수학으로 거슬러 올라가며 항상 단순화를 추구합니다. Go의 `math.Round`는 [은행가의 반올림](https://en.wikipedia.org/wiki/Rounding#Round_half_to_even)을 사용하는데, 이는 0.5가 가장 가까운 짝수로 반올림되어, 합계에 영향을 줄 수 있는 편향을 줄입니다.

부동 소수점 숫자는 이진 표현 때문에 까다로울 수 있으며, 모든 소수를 정확히 나타내지 못할 수 있습니다. 그러나, Go의 접근 방식은 대부분의 경우 예상된 동작을 유지합니다.

"반올림 반가산" 또는 "0으로부터 멀어지게 반올림" 같은 다른 반올림 방법이 존재하지만, Go의 표준 라이브러리가 즉시 사용 가능한 것입니다. 더 복잡한 필요사항을 위해서는 타사 라이브러리나 자체 해결책을 찾을 수도 있습니다.

## 참고하십시오
- Go의 `math` 패키지: [https://pkg.go.dev/math](https://pkg.go.dev/math)
- 부동 소수점 연산을 위한 IEEE 754 표준 (Go가 부동 소수를 처리하는 기반): [https://ieeexplore.ieee.org/document/4610935](https://ieeexplore.ieee.org/document/4610935)
- 부동 소수점 이해하기: ["모든 컴퓨터 과학자가 부동 소수점 연산에 대해 알아야 할 것"](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
