---
title:                "임의의 숫자 생성하기"
html_title:           "Elixir: 임의의 숫자 생성하기"
simple_title:         "임의의 숫자 생성하기"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 필요한가?

랜덤 숫자 생성이란, 컴퓨터에서 예측이 불가능한 숫자를 생성하는 것입니다. 이것은 보안, 새로운 데이터 생성, 게임 개발 등의 목적에 사용될 수 있습니다.

## 어떻게 하는가:

```Go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

func main() {
    // 시드값으로 현재 시간을 사용
    rand.Seed(time.Now().UnixNano())
    fmt.Println(rand.Intn(100)) // 0 ~ 99 사이의 랜덤한 값 출력
}
```
위 예제를 실행하면, 0과 99 사이의 랜덤한 숫자를 생성합니다.

## 깊이 있게 알아보기:

랜덤 숫자 생성은 알고리즘에 의해 이루어지므로 엄밀히 말하면 이는 완전히 무작위가 아닙니다. 이를 의사 랜덤 숫자(Pseudorandom number)라 하며, 알고리즘이 시작하는 시드값에 따라 동일한 시리즈의 숫자가 생성됩니다.

Go 언어에서는 `math/rand` 패키지를 사용해 가장 일반적인 의사 랜덤 숫자를 생성할 수 있습니다. 물론 이외에도, CSPRNG(Cryptographically Secure Pseudorandom Number Generator)와 같이 보안에 좀 더 적합한 `crypto/rand` 패키지를 사용하는 등 다양한 방법이 존재합니다.

## 참고 자료:

- Go 공식 문서에서 `math/rand` 패키지에 대한 더 깊은 이해를 얻을 수 있습니다: https://golang.org/pkg/math/rand/
- `crypto/rand` 패키지에 대한 자세한 내용은 다음을 참조하세요: https://golang.org/pkg/crypto/rand/