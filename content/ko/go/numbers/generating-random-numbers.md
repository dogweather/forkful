---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:57:26.830281-07:00
description: "\uC5B4\uB5BB\uAC8C: Go\uC5D0\uC11C\uB294 `math/rand` \uD328\uD0A4\uC9C0\
  \uB97C \uC0AC\uC6A9\uD558\uC5EC \uC758\uC0AC \uB09C\uC218\uB97C \uC0DD\uC131\uD558\
  \uAC70\uB098 `crypto/rand`\uB97C \uC0AC\uC6A9\uD558\uC5EC \uC554\uD638\uD654\uC5D0\
  \ \uC548\uC804\uD55C \uC758\uC0AC \uB09C\uC218\uB97C \uC0DD\uC131\uD569\uB2C8\uB2E4\
  . \uB450 \uAC00\uC9C0\uB97C \uBAA8\uB450 \uD0D0\uAD6C\uD574\uBD05\uC2DC\uB2E4. \uBA3C\
  \uC800 `math/rand` \uD328\uD0A4\uC9C0\uC640 \uBC1C\uC0DD\uAE30\uB97C \uCD08\uAE30\
  \uD654\uD558\uB294 \uB370 \uD544\uC694\uD55C `time` \uD328\uD0A4\uC9C0\uB97C\u2026"
lastmod: '2024-03-13T22:44:54.448477-06:00'
model: gpt-4-0125-preview
summary: "Go\uC5D0\uC11C\uB294 `math/rand` \uD328\uD0A4\uC9C0\uB97C \uC0AC\uC6A9\uD558\
  \uC5EC \uC758\uC0AC \uB09C\uC218\uB97C \uC0DD\uC131\uD558\uAC70\uB098 `crypto/rand`\uB97C\
  \ \uC0AC\uC6A9\uD558\uC5EC \uC554\uD638\uD654\uC5D0 \uC548\uC804\uD55C \uC758\uC0AC\
  \ \uB09C\uC218\uB97C \uC0DD\uC131\uD569\uB2C8\uB2E4."
title: "\uB79C\uB364 \uC22B\uC790 \uC0DD\uC131\uD558\uAE30"
weight: 12
---

## 어떻게:
Go에서는 `math/rand` 패키지를 사용하여 의사 난수를 생성하거나 `crypto/rand`를 사용하여 암호화에 안전한 의사 난수를 생성합니다. 두 가지를 모두 탐구해봅시다.

### `math/rand`를 사용하여 의사 난수 생성
먼저 `math/rand` 패키지와 발생기를 초기화하는 데 필요한 `time` 패키지를 import합니다. 초기화는 매 실행마다 다른 숫자의 순서를 얻기 위해 필요합니다.

```go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	rand.Seed(time.Now().UnixNano())
	fmt.Println("임의의 숫자:", rand.Intn(100)) // 0과 99 사이의 숫자를 생성합니다
}
```

샘플 출력: `임의의 숫자: 42`

### `crypto/rand`를 사용하여 암호화에 안전한 의사 난수 생성
보안에 민감한 애플리케이션의 경우, 예측하기 어려운 난수를 생성하는 `crypto/rand` 패키지가 적합하며, 이는 암호화 작업에 적합합니다.

```go
package main

import (
	"crypto/rand"
	"fmt"
	"math/big"
)

func main() {
	n, _ := rand.Int(rand.Reader, big.NewInt(100))
	fmt.Println("안전한 임의의 숫자:", n)
}
```

샘플 출력: `안전한 임의의 숫자: 81`

## 심층 분석
Go의 `math/rand`와 `crypto/rand` 패키지 사이의 핵심 차이점은 그들의 엔트로피 소스와 의도된 사용 사례에서 비롯됩니다. `math/rand`는 초기 시드를 기반으로 의사 난수를 생성하며, 따라서 시퀀스는 결정론적이고 시드가 알려진 경우 예측될 수 있습니다. 이는 시뮬레이션 또는 게임과 같이 절대적인 예측불가성이 아니라 높은 성능이 주요 관심사인 시나리오에 적합합니다.

반면, `crypto/rand`는 기본 운영 체제로부터 무작위성을 도출하여 예측불가성이 중요한 암호화 용도에 적합합니다. 그러나 이는 성능과 생성된 숫자를 처리하는 복잡성(예: 정수의 경우 `*big.Int` 타입 처리)의 비용을 수반합니다.

역사적으로 컴퓨터에서 난수 생성의 개념은 항상 진정한 "무작위성"의 가장자리에서 춤을 추어왔으며, 초기 시스템은 무작위성을 모방하는 결정론적 알고리즘에 크게 의존했습니다. 컴퓨터가 발전함에 따라 이러한 알고리즘도 그 환경에서 더 정교한 엔트로피 소스를 취합하여 발전하였습니다.

이러한 발전에도 불구하고, 컴퓨터 자체의 결정론적 특성을 고려할 때 완벽한 무작위성을 컴퓨팅에서 추구하는 것은 본질적으로 역설적입니다. 이것이 대부분의 애플리케이션에서 예측성이 해로울 경우에도, 그 오버헤드에도 불구하고 `crypto/rand`와 같은 소스에서 나오는 암호화에 안전한 의사 난수가 더 나은 대안이 되는 이유입니다.

본질적으로, Go는 성능과 보안 사이의 타협을 우아하게 해결하는 두 개의 구별된 패키지를 가지고 있어 개발자들이 자신의 특정 필요에 따라 선택할 수 있게 합니다.
