---
title:    "Go: 랜덤 숫자 생성"
keywords: ["Go"]
---

{{< edit_this_page >}}

## 왜
랜덤한 숫자를 생성하는 것에 참여해야 하는 *이유*에 대해 알아보겠습니다. 

랜덤한 숫자를 생성하는 것은 프로그래밍에서 매우 유용합니다. 예를 들어, 랜덤한 숫자를 생성해서 게임에서 사용할 수 있습니다. 또는 보안 관련 작업에서 랜덤한 숫자가 필요한 경우도 있습니다. 이러한 이유로, 랜덤한 숫자 생성은 많은 개발자에게 필수적인 작업입니다.

## 방법
아래의 코드 블록을 참고하여 랜덤한 숫자를 생성하는 방법을 간단하게 알아보겠습니다.

```Go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

func main() {
    rand.Seed(time.Now().UnixNano()) // 현재 시간을 기준으로 시드 설정
    num := rand.Intn(10) // 0부터 10 미만의 수 중 랜덤하게 생성
    fmt.Println("랜덤한 숫자:", num)
}
```
출력: 랜덤한 숫자: 6

위의 코드에서 `rand.Seed()` 함수를 사용하여 현재 시간을 시드 값으로 설정합니다. 그리고 `rand.Intn()` 함수를 사용하여 0부터 10 미만의 랜덤한 숫자를 생성합니다. 결과적으로 `6`이 출력됩니다. 

물론 위의 예제는 간단한 예제이므로 다양한 방식으로 랜덤한 숫자를 생성할 수 있습니다. Go 언어에서 제공하는 `rand` 패키지를 자세히 살펴보면 더 많은 방법을 알 수 있습니다.

## 딥 다이브
Go 언어에서 제공하는 `rand` 패키지는 난수 생성기를 구현할 때 매우 유용합니다. 이 패키지는 여러 가지 난수 생성기를 구현하는 함수들을 제공합니다. 예를 들어, `Intn()` 함수는 정수형 난수를 생성하는 데 사용되며, `Float64()` 함수는 실수형 난수를 생성하는 데 사용됩니다.

또한 `rand` 패키지는 시드 값을 설정하는 함수들도 제공합니다. 시드 값을 설정하는 것은 랜덤한 숫자를 생성할 때 매우 중요한 역할을 합니다. 이를 통해 더 높은 수준의 랜덤성을 보장할 수 있습니다.

더 많은 정보를 알고 싶다면 공식 문서를 참고하시기 바랍니다.

[Go 언어 공식 문서](https://golang.org/pkg/math/rand/)

## 참고
* [Go 언어 공식 문서](https://golang.org/pkg/math/rand/)
* [Tutorialspoint - Go 난수 생성기](https://www.tutorialspoint.com/go/go_random_number.htm)
* [Medium - Go 언어에서 난수 생성기 사용하기](https://medium.com/learning-the-go-programming-language/pseudo-random-number-generation-in-golang-c548eb20485e)