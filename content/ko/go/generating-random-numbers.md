---
title:                "난수 생성"
date:                  2024-01-27T20:33:58.265071-07:00
model:                 gpt-4-0125-preview
simple_title:         "난수 생성"

category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

Go에서 난수를 생성하는 것은 `math/rand` 패키지를 사용하여 실험 시뮬레이션, 테스트 데이터 생성 또는 게임에 예측 불가능성을 추가하는 등 다양한 응용 프로그램을 위한 의사 난수를 생성하는 것을 포함합니다. 프로그래머는 이 기능을 사용하여 동적이고 예측 가능성이 적은 소프트웨어 동작을 만듭니다.

## 방법:

Go에서 난수 생성을 시작하려면 `math/rand` 패키지와 더 많은 예측 불가능성을 위해 난수 생성기를 시드하는 `time` 패키지를 가져와야 합니다. 기본 예는 다음과 같습니다:

```Go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	// 생성기 시드
	rand.Seed(time.Now().UnixNano())
	
	// 0과 99 사이의 무작위 정수 생성
	randomInt := rand.Intn(100)
	fmt.Println("Random Integer:", randomInt)
	
	// 0.0과 1.0 사이의 무작위 부동 소수점 생성
	randomFloat := rand.Float64()
	fmt.Println("Random Float:", randomFloat)
}
```

예제 출력은 다음과 같이 나타날 수 있습니다:

```
Random Integer: 42
Random Float: 0.7304601899194229
```

각 실행은 현재 시간으로 시딩하기 때문에 다른 숫자를 생성한다는 것을 기억하세요.

## 심층 분석

Go의 `math/rand` 패키지는 다양한 분포를 위한 의사 난수 생성기(Pseudo-Random Number Generators, PRNGs)를 구현합니다. 많은 응용 프로그램에서 매우 효과적이지만, `math/rand`에 의해 생성된 숫자는 결정론적인 성격 때문에 암호화 목적에는 적합하지 않다는 점을 명심하는 것이 중요합니다. 암호화에 필요한 경우, 안전한 난수 생성기를 제공하는 `crypto/rand` 패키지가 적절한 선택입니다.

`math/rand`의 구현은 빼기 기반의 난수 생성기 알고리즘에 기반을 두고 있으며, 이는 효율적이고 순서가 반복되기 전에 상대적으로 긴 주기를 가집니다. 그러나 암호화 작업과 같이 진정으로 무작위 시퀀스가 필요한 응용 프로그램의 경우, 하드웨어 난수 생성기(RNGs) 또는 시스템 특정 안전 무작위성 소스와 인터페이스하는 `crypto/rand` 패키지가 권장됩니다.

`math/rand`는 가변성을 도입하기 위해 시딩을 허용하지만, 동일한 시드는 항상 동일한 숫자 시퀀스를 생성함으로써 그 결정론적인 무작위성의 성격을 강조합니다. 이는 디버깅 또는 테스트 목적으로 재현성이 바람직할 수 있는 시뮬레이션 또는 게임에 적합합니다.
