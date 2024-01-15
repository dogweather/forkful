---
title:                "랜덤 숫자 생성"
html_title:           "Go: 랜덤 숫자 생성"
simple_title:         "랜덤 숫자 생성"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜
랜덤 숫자를 생성하는 것은 다양한 분야에서 매우 유용합니다. 무작위 숫자를 통해 게임, 통계 및 암호화 등 다양한 분야에서 사용할 수 있기 때문입니다.

## 방법
```Go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	// 현재 시간을 기반으로 시드 생성
	rand.Seed(time.Now().UnixNano())

	// 0에서 10까지의 무작위 정수 생성
	randomNum := rand.Intn(11)

	fmt.Println("0에서 10 사이의 무작위 숫자:", randomNum)
}
```

출력:
```
0에서 10 사이의 무작위 숫자: 7
```

이 코드의 주석을 따라가며, 우리는 먼저 `math/rand` 패키지를 임포트합니다. 이 패키지는 무작위 숫자를 생성하는 데 필요한 함수들을 제공합니다. 그리고 현재 시간을 기반으로 시드를 생성하여 무작위성을 더해 줍니다. 마지막으로 `Rand.Intn()` 함수를 사용하여 0에서 10까지의 무작위 정수를 생성하고 출력합니다.

## 심층 분석
랜덤 숫자를 생성하는 방법에는 여러 가지가 있지만, Go 언어에서 제공하는 `math/rand` 패키지를 사용하는 것이 가장 간편합니다. 이 패키지에는 다양한 타입의 무작위 숫자를 생성할 수 있는 함수들이 있습니다. 올바른 시드를 제공하고 적절한 함수를 사용하면 좀 더 신뢰할 수 있는 무작위성을 얻을 수 있습니다.

## 참고
- [Go 언어 공식 문서 - math/rand 패키지](https://golang.org/pkg/math/rand/)
- [GolangCode - How to generate a random number](https://golangcode.com/random-number-generation/)