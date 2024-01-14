---
title:                "Go: 난수 생성하기"
simple_title:         "난수 생성하기"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜
난수 생성에 참여하는 이유는 다양하다. 예를 들어, 임의의 숫자를 사용해 게임을 만들거나 암호를 생성하고자 할 때, 난수 생성이 필요하다. 또한 데이터를 무작위로 섞는 등의 작업에도 난수 생성이 필요하다. 간단하게 말해, 난수 생성은 컴퓨터 프로그래밍에서 필수적인 부분이다.

## 사용 방법
Go 언어에서 난수를 생성하는 방법은 매우 간단하다. `math/rand` 패키지를 import하고 `rand.Intn()` 함수를 사용하여 원하는 범위의 난수를 생성할 수 있다. 아래는 1부터 10까지의 난수를 생성하는 예제이다.

```Go
package main

import (
	"fmt"
	"math/rand"
)

func main() {
	num := rand.Intn(10) + 1
	fmt.Println(num)
}
```

위 코드를 실행하면 매번 다른 숫자가 출력된다. 또한, `rand.Float64()` 함수를 사용하면 0.0부터 1.0 사이의 실수형 난수를 생성할 수도 있다.

```Go
package main

import (
	"fmt"
	"math/rand"
)

func main() {
	num := rand.Float64()
	fmt.Println(num)
}
```

## 깊게 파고들기
실제로 난수를 생성하는 방법은 간단하지만, 그 내부에서 사용되는 알고리즘은 복잡하다. Go 언어에서는 기본적으로 메르센 트위스터(Mersenne Twister) 알고리즘을 사용하여 난수를 생성한다. 이 알고리즘은 이론적으로 최초 6,711,727개의 수열이 겹치지 않는 난수를 생성할 수 있다고 알려져 있다.

따라서 Go에서 제공하는 기본 난수 생성 함수는 충분히 무작위성을 보장하기에 충분하다고 할 수 있다. 하지만, 보안적인 목적이나 더 강력한 난수가 필요한 경우에는 다른 알고리즘을 사용해야 한다.

## 관련 링크
- [메르센 트위스터(Mersenne Twister)](https://ko.wikipedia.org/wiki/%EB%A9%94%EB%A5%B4%EC%84%BC_%ED%8A%B8%EC%9C%84%EC%8A%A4%ED%84%B0)
- [Go 언어 공식 문서 - 난수 생성기](https://golang.org/pkg/math/rand/)
- [난수 생성 알고리즘 비교](https://velog.io/@sungjunyoung/comparing-random-number-generators)

## 참고 자료
- [Go 언어 기초 문법](https://wikidocs.net/42162)
- [Go 언어 공식 문서](https://golang.org/doc/)
- [Go 언어 용어사전](https://go-dictionary.org/)