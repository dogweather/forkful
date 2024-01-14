---
title:                "Go: 문자열 연결하기"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜

문자열 연결을 하는 이유는 문자열을 동적으로 만들어내기 위해서입니다.

## 하는 방법

Go 언어에서 문자열을 연결하는 가장 기본적인 방법은 `+` 연산자를 사용하는 것입니다. 예를 들어, `Hello`와 `world` 두 개의 문자열을 연결하면 `Helloworld`가 됩니다.

```Go
package main

import "fmt"

func main() {
	message := "Hello" + "world"
	fmt.Println(message)
}

// Output:
// Helloworld
```

또 다른 방법으로는 `fmt` 패키지에서 제공하는 `Sprintf()` 함수를 사용하는 것입니다. 이 함수는 서식 지정된 문자열을 반환하기 때문에 변수에 할당해야 합니다.

```Go
package main

import "fmt"

func main() {
	name := "John"
	message := fmt.Sprintf("Hello %s!", name)
	fmt.Println(message)
}

// Output:
// Hello John!
```

## 깊게 들어가기

Go 언어에서 연결될 문자열의 개수가 많아질수록 `+` 연산자를 사용하는 것 보다는 `Sprintf()` 함수를 사용하는 것이 더 효율적입니다. `+` 연산자는 문자열을 연결하기 위해 매번 새로운 문자열을 생성하기 때문에 메모리 관리 측면에서 비효율적입니다. 하지만 `Sprintf()` 함수를 사용하면 특정 서식을 지정하여 문자열을 만들 수 있기 때문에 더 효율적입니다.

또한 Go 언어에서 문자열은 불변 값입니다. 따라서 문자열을 연결할 때마다 새로운 문자열이 생성되는 것이 아니라, 메모리에 새로운 주소 공간을 할당하고 문자열을 복사함으로써 연결되는 것입니다.

## See Also

- [Go 언어 공식 문서](https://golang.org/doc/)
- [Effective Go](https://golang.org/doc/effective_go.html)
- [A Tour of Go](https://tour.golang.org/)