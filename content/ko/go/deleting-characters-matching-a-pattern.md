---
title:    "Go: 패턴과 일치하는 문자 삭제하기"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜

가끔 우리는 소프트웨어에서 특정 패턴과 일치하는 문자를 삭제해야 할 때가 있습니다. 이런 작업을 하는 이유는 다양합니다. 예를 들어, 사용자 입력에서 특정한 문자를 제거하거나, 문자열에서 중복된 문자를 제거할 수도 있습니다.

하지만 이 작업을 수작업으로 하기에는 너무 번거롭고 시간이 오래 걸릴 수 있습니다. 이럴 때에는 Go 언어에서 제공하는 강력한 문자열 조작 기능을 이용하여, 쉽고 빠르게 문자를 삭제할 수 있습니다.

## 방법

"```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    // 문자열을 생성합니다.
    str := "이것은Go프로그래밍입니다.Go언어는매우필요합니다."

    // strings 패키지의 ReplaceAll 함수를 이용하여, "Go"라는 문자열을 모두 ""로 대체합니다.
    result := strings.ReplaceAll(str, "Go", "")

    // 결과를 출력합니다.
    fmt.Println(result)
}
```

이렇게 단 몇 줄의 코드만으로도, 입력된 문자열에서 "Go"라는 문자를 모두 삭제할 수 있습니다. 그리고 이 기능을 응용하여 다양한 패턴의 문자를 삭제할 수도 있습니다.

## 깊이 있는 설명

Go 언어에서는 문자열을 다루는데 매우 편리한 기능들을 제공합니다. ReplaceAll 함수를 비롯하여, strings 패키지에는 문자열 조작에 유용한 다양한 함수들이 포함되어 있습니다. 예를 들어, 문자열을 대문자로 변환하거나, 문자열을 분할하거나, 부분 문자열을 교체하는 등의 기능을 제공합니다.

또한, 정규식을 이용하여 패턴을 지정하여 문자열을 조작하는 기능도 제공되며, 이를 이용하면 더욱더 다양한 문자열 삭제가 가능합니다.

## 또 다른 자료

- https://golang.org/pkg/strings/
- https://golang.org/pkg/regexp/
- https://blog.golang.org/strings