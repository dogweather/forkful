---
title:                "패턴과 일치하는 문자 삭제하기"
html_title:           "Go: 패턴과 일치하는 문자 삭제하기"
simple_title:         "패턴과 일치하는 문자 삭제하기"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# 왜

캐릭터 매칭 패턴을 삭제하는 작업을 수행하는 이유는 문자열에서 특정 패턴을 제거하고 원하는 형태의 새로운 문자열을 만들기 위해서입니다.

## 하우 투

패턴 매칭을 통해 문자열에서 특정 문자를 삭제하는 작업은 Go의 `strings` 패키지의 `ReplaceAll` 함수를 사용하여 간단하게 수행할 수 있습니다.

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "Hello World!"
    newStr := strings.ReplaceAll(str, "l", "")
    fmt.Println(newStr) // 출력: Heo Word!
}
```

위 예시에서는 `ReplaceAll` 함수를 사용하여 `l` 문자를 모두 찾아 삭제한 후, 새로운 문자열을 출력하고 있습니다. 따라서 패턴 매칭을 통해 문자를 삭제할 수 있습니다.

## 딥 다이브

Go의 `strings` 패키지에는 `ReplaceAll` 함수 외에도 다양한 함수들이 존재합니다. 예를 들어, `Replace` 함수는 대소문자를 구분하여 매칭하는 기능을 제공하고, `Trim` 함수는 문자열의 특정 부분을 삭제할 수 있도록 도와줍니다.

또한 Go에서는 정규식(Regular Expressions)을 사용하여 패턴 매칭을 수행할 수도 있습니다. `regexp` 패키지를 import한 뒤, `ReplaceAllString` 함수를 사용하여 정규식을 적용한 패턴 매칭을 수행할 수 있습니다.

See Also:

- `strings` 패키지 문서: https://golang.org/pkg/strings/
- `regexp` 패키지 문서: https://golang.org/pkg/regexp/