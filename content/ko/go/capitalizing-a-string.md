---
title:                "Go: 문자열의 대문자화"
simple_title:         "문자열의 대문자화"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜

Go 언어를 사용해 문자열을 대문자로 바꾸려는 이유는 다양합니다. 예를 들면 사용자의 입력이나 파일 이름을 대문자로 통일하기 위해서, 또는 출력이나 비교를 위해 대문자로 바꾸는 등의 다양한 이유가 있을 수 있습니다. 

## 하는 법

```Go
package main

import (
	"fmt"
	"strings"
)

func uppercase(str string) string {
	return strings.ToUpper(str)
}

func main() {
	input := "hello go"
	
	output := uppercase(input)
	fmt.Println(output)
}
```

출력 결과: HELLO GO 

위의 예제에서는 `strings` 패키지의 `ToUpper` 함수를 사용하여 문자열을 대문자로 바꾸는 방법을 보여줍니다. 이 외에도 `strings` 패키지에는 문자열을 소문자로 바꾸는 함수인 `ToLower`도 있습니다. 또는 `bytes` 패키지의 `ToUpper`와 `ToLower` 함수는 문자열 대신 바이트 슬라이스를 대상으로 동작하므로 더 빠를 수 있습니다. 

## 딥 다이브

문자열을 대문자로 바꾸기 위해서는 문자 조작에 대한 이해가 필요합니다. Go 언어는 문자 하나를 유니코드 코드 포인트로 다룹니다. 즉, 문자 하나에 해당하는 코드 값이 할당되며 이를 통해 대소문자를 구분할 수 있습니다. 대문자는 유니코드 코드 포인트 값이 작은 숫자로 할당되어 있기 때문에 `ToLower` 함수를 사용하면 소문자로 바꿀 수 있습니다. 

하지만 한글의 경우에는 조금 다릅니다. 한글은 합성 문자로 이루어져 있으며 하나의 음절을 표현하기 위해 여러 개의 코드 포인트가 필요합니다. 따라서 `ToLower` 함수를 사용하면 의도하지 않은 결과가 나올 수 있습니다. 이러한 경우에는 유니코드 표준에서 제공하는 관련 함수를 사용하거나 다른 라이브러리를 사용하는 것이 좋습니다. 

## 참고

- [Go 언어 공식 문서](https://golang.org/pkg/strings/#ToUpper)
- [유니코드 표준](https://unicode.org/)
- [Go 언어 문자열 조작 라이브러리 - GoKo](https://github.com/verizip/goko)