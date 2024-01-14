---
title:    "Go: 문자열 대문자화"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열을 대문자로 바꾸는 것의 중요성을 간단하게 소개해보겠습니다. 왜냐하면 Go 언어는 대소문자를 엄격하게 구분하기 때문에, 문자열을 대문자로 바꾸지 않으면 예상치 못한 오류가 발생할 수 있습니다. 또한 대문자는 소문자보다 더 강조될 때가 있으므로 프로그램에서 문자열을 대문자로 변환하면 더욱 명확한 메시지를 전달할 수 있습니다.

## 어떻게 하나요?

문자열을 대문자로 바꾸는 가장 간단한 방법은 `strings` 패키지의 `ToUpper` 함수를 사용하는 것입니다. 이 함수는 문자열의 모든 문자를 대문자로 변환하고 변환된 문자열을 반환합니다. 아래는 이 함수를 사용하는 예시 코드와 결과물입니다.

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "hello, world!"
    fmt.Println(strings.ToUpper(str))
}
```

출력결과:

```
HELLO, WORLD!
```

위에서 볼 수 있듯이 `strings.ToUpper` 함수를 사용하면 간단하게 문자열을 대문자로 변환할 수 있습니다.

## 깊게 파헤쳐보기

하지만 `strings.ToUpper` 함수 하나만으로는 문자열을 대문자로 바꾸는 과정을 완전히 이해하기는 어렵습니다. 따라서 이번 섹션에서는 문자열을 대문자로 변환하는 과정을 더 깊게 파헤쳐보겠습니다.

문자열은 바이트(byte) 값의 슬라이스(slice)로 이루어져 있습니다. 따라서 문자열을 대문자로 변환하려면 문자열을 바이트 슬라이스로 변환한 후, 각 바이트 값에 해당하는 유니코드 문자에 대한 대문자 값을 계산해야 합니다.

아래는 이 과정을 수행하는 간단한 예시 코드입니다.

```Go
package main

import (
    "fmt"
    "strings"
    "unicode"
)

func main() {
    str := "hello, world!"
    
    // 문자열을 바이트 슬라이스로 변환
    bytes := []byte(str)
    
    // 각 바이트 값에 해당하는 유니코드 문자에 대한 대문자 값을 계산
    for i, b := range bytes {
        bytes[i] = byte(unicode.ToUpper(rune(b)))
    }
    
    // 변환된 바이트 슬라이스를 문자열로 다시 변환하여 출력
    fmt.Println(string(bytes))
}
```

출력결과:

```
HELLO, WORLD!
```

위 코드에서는 `unicode.ToUpper` 함수를 사용하여 각 바이트 값에 해당하는 대문자 값을 계산하였습니다. 이 함수는 `rune` 타입의 값을 인자로 받아 해당하는 대문자 값을 반환합니다.

## 더 알아보기

Go 언어의 `strings` 패키지에는 대문자로 변환하는 `ToUpper` 함수 외에도 소문자로 변환하는 `ToLower` 함수 등 문자열을 다루는 다양한 유용한 함수가 포함되어 있습니다. 이 외에도 `unicode` 패키지의 함수들을 이용하여 문자열을 변환하는 방법을 배울 수 있으니 관련 문서를 참조해보시기 바랍니다.

## 또한 참고해보세요

- [Go 언어 `strings` 패키지 문서](https://golang.org/pkg/strings/)
- [Go 언어 `unicode` 패키지 문서](https://golang.org/pkg/unicode/)