---
title:                "Go: 랜덤 숫자 생성하기"
programming_language: "Go"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 왜 랜덤한 숫자를 생성하는가?

랜덤한 숫자 생성은 프로그래밍에서 매우 유용합니다. 여러 가지 이유로 랜덤한 숫자를 생성해야 할 수 있지만, 주로 게임, 시뮬레이션, 보안 등에서 사용됩니다. 이 글에서는 Go 언어를 사용하여 랜덤한 숫자를 생성하는 방법에 대해 알아보겠습니다.

## 방법

Go 언어에서는 "math/rand" 패키지를 사용하여 랜덤한 숫자를 생성할 수 있습니다. 먼저 "math/rand" 패키지를 임포트합니다.

```Go
import "math/rand"
```

랜덤한 숫자를 생성하기 위해서는 seed 값이 필요합니다. 이는 일종의 시작점으로서 매번 실행될 때매 다른 랜덤한 숫자를 생성하기 위해 사용됩니다. 시간을 기준으로 seed 값을 생성하려면 "time" 패키지를 임포트해야 합니다.

```Go
import (
	"math/rand"
	"time"
)
```

seed 값을 생성한 후에는 "rand.Intn(n)" 함수를 사용하여 0부터 n-1 사이의 랜덤한 숫자를 생성할 수 있습니다.

```Go
rand.Seed(time.Now().UnixNano()) // 시간을 기준으로 seed 값 생성
randomNumber := rand.Intn(10) // 0부터 9까지의 랜덤한 숫자 생성
fmt.Println(randomNumber) // 예시 출력: 5
```

위의 예시에서는 0부터 9까지의 랜덤한 숫자를 생성하도록 설정했지만, 필요에 따라서 다른 범위의 숫자도 생성할 수 있습니다.

또한, "math/rand" 패키지는 유사한 난수를 생성하는 "rand.Read()" 함수도 제공합니다. 이 함수는 입력으로 들어오는 바이트 슬라이스를 채우기 위해 암호학적으로 안전한 난수를 생성합니다. 이 방법은 좀 더 보안적인 요구사항이 있는 경우에 유용하게 사용될 수 있습니다.

## 깊게 파헤치기

Go 언어에서 랜덤한 숫자를 생성하는 방법은 여러 가지이지만, 일반적으로 가장 많이 사용되는 방법은 "math/rand" 패키지의 "rand.Intn(n)" 함수를 사용하는 것입니다. 이 함수는 입력으로 주어지는 숫자보다 작은 범위의 랜덤한 숫자를 생성합니다. 이 함수는 내부적으로 선형 합동 생성기(Linear Congruential Generator)를 사용하여 직렬적인 seed 값을 생성하고 변환합니다. 이는 일련의 수학적 계산을 통해 이전의 값을 바탕으로 다음 값을 생성하여 랜덤한 숫자를 생성하는 방식입니다.

하지만 만약 암호학적으로 안전한 랜덤한 숫자가 필요하다면 "math/rand" 패키지의 "rand.Read()" 함수를 사용하는 것이 좋습니다. 이 함수는 알고리즘으로서 "ChaCha20"를 사용하여 임의의 바이트 슬라이스를 채우기 때문에 매우 보안적으로 안전한 방법입니다.

## 관련 링크

- [Go 언어 공식 문서에서 "math/rand" 패키지 소개] (https://golang.org/pkg/math/rand/)
- [안전성에 대한 고찰: Linear Congruential Generator vs ChaCha20] (https://codeburst.io/safety-in-go-generating-random-numbers-ebe625e4f638)
- [Go 언어 공식 문서에서 "time"