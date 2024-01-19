---
title:                "문자열 보간하기"
html_title:           "Clojure: 문자열 보간하기"
simple_title:         "문자열 보간하기"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열 보간이라는 것은 변수의 값이나 표현식의 결과를 문자열에 끼워 넣는 기법입니다. 프로그래머들이 이를 사용하는 이유는 코드의 가독성 높이고 코드의 길이를 줄이기 위해서입니다.

## 어떻게 하는가?

Go 언어에서는 `fmt.Sprintf()` 를 이용하여 문자열을 보간할 수 있습니다:

```Go
package main

import "fmt"

func main() {
   name := "Kim"
   fmt.Printf("Hello, %s!\n", name)
}
```

출력:

`Hello, Kim!`

## 깊이 있는 정보

- **역사적 맥락**: 문자열 보간은 개발이 이루어지는 동안 동적으로 문자열을 구성하는 데 사용되었습니다.
- **대안**: Go 언어에서 문자열 보간 외에 문자열 결합을 위해 `+` 연산자를 사용할 수 있습니다.
- **구현 내용**: `fmt.Sprintf()`는 내부적으로 문자열 변환을 의미하며, 각 변환에서 해당 인수의 타입에 따라 결과가 달라집니다.

```Go
package main

import "fmt"

func main() {
   name := "Kim"
   fmt.Println("Hello, " + name + "!")
}
```

출력:

`Hello, Kim!`

## 추가로 살펴보기

- 문자열 보간에 대한 자세한 정보는 [Go 공식 문서](https://golang.org/pkg/fmt/)에서도 확인할 수 있습니다.
- [Go By Example: String Formatting](https://gobyexample.com/string-formatting)에서는 문자열 포맷 및 보간에 대한 다양한 예제를 제공합니다.