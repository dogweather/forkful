---
title:                "난수 생성하기"
date:                  2024-01-20T17:49:22.307015-07:00
model:                 gpt-4-1106-preview
simple_title:         "난수 생성하기"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가요? 왜 사용하죠?)
랜덤 숫자를 생성하는 것은 예측할 수 없는 숫자를 만드는 과정입니다. 프로그래머들은 게임, 시뮬레이션, 보안 등 다양한 상황에서 무작위성이 필요할 때 이를 사용합니다.

## How to: (어떻게 만드나요?)
```Go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	rand.Seed(time.Now().UnixNano()) // 시드 설정
	randomNumber := rand.Intn(100)   // 0-99 사이의 랜덤 숫자 생성
	fmt.Println(randomNumber)        // 랜덤 숫자 출력
}
```
예상 출력:
```
42
```

## Deep Dive (깊은 고찰)
랜덤 숫자 생성에는 역사적으로 많은 관심이 있습니다. 초기 컴퓨터에서는 완벽한 랜덤성을 달성하기 어려웠지만, 시간이 지나면서 알고리즘은 계속해서 개선되었습니다. Go에서는 `math/rand` 패키지가 이 작업을 담당합니다. 정말 랜덤한 값이 아니라 "의사(pseudo)" 랜덤입니다—`rand.Seed` 함수를 사용해 시드를 설정하는 것은 예측 불가능한 시퀀스를 생성하는 데 도움이 됩니다. 그러나, 진정한 보안이 필요한 애플리케이션의 경우 `crypto/rand` 패키지가 더 적합한 선택일 수 있습니다.

## See Also (더 보기)
- Go 공식 문서의 `math/rand` 패키지: https://golang.org/pkg/math/rand/
- 암호화 안전 랜덤 생성기에 대한 `crypto/rand` 패키지: https://golang.org/pkg/crypto/rand/
- 랜덤 숫자 생성기(RNG)의 역사에 대해 더 알아보기: https://en.wikipedia.org/wiki/Random_number_generation
