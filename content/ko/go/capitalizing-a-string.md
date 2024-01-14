---
title:    "Go: 문자열 대문자로 바꾸기"
keywords: ["Go"]
---

{{< edit_this_page >}}

## 왜? 

문자열을 대문자로 바꾸는 것의 이점은 명확하게 프로그램에서 문자열을 구분하기 위함이다. 대문자는 소문자와 다르게 보이기 때문에, 읽기 쉬운 코드를 작성하는 데에 유용하다.

## 방법: 

우선, 문자열을 입력받는 메소드를 사용한다. 그 후, 문자열의 각 문자를 대문자로 바꾸는 반복문을 작성한다. 이후 바뀐 문자열을 출력한다. 아래는 Go언어로 작성된 코드 예시와 해당 코드의 출력 결과이다.

```
Go를 이용한 문자열 대문자로 바꾸기 코드 예시:
package main 

import (
    "fmt"
    "strings"
)

func main() {
    userInput := "hello world" 
    upperCaseString := strings.ToUpper(userInput) 

    fmt.Println(upperCaseString)
}

출력: HELLO WORLD 
```

## 깊게 파헤치기: 

Go언어에서는 기본적으로 제공하는 `strings` 패키지의 `ToUpper` 메소드를 사용하여 문자열을 대문자로 바꿀 수 있다. 이 메소드는 문자열을 전달받고 해당 문자열의 각 문자를 대문자로 바꾸어준다. 물론, 이 메소드는 유니코드 문자열도 지원한다.

## 참고: 

- [Go언어 공식 문서 - strings 패키지](https://golang.org/pkg/strings/)
- [Go언어 공식 문서 - 문자열에 대한 자세한 정보](https://golang.org/ref/spec#String_types)
- [모든 프로그래머를 위한 Go언어 - 문자열 프로그래밍](https://medium.com/@jimmy_garden/study-go-string-programming-c40cbf596940)