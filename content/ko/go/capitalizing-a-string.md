---
title:                "스트링 대문자로 변경하기"
html_title:           "Go: 스트링 대문자로 변경하기"
simple_title:         "스트링 대문자로 변경하기"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열을 대문자로 변환하는 것이 왜 중요한지 궁금하지 않으신가요? 예를 들어, 사용자가 입력한 문자열을 대문자로 변환해서 저장하면, 동일한 입력이라도 대문자와 소문자를 구분하지 않는 검색 기능을 구현할 수 있습니다.

## 방법

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "hello world"
	result := strings.ToUpper(str)
	fmt.Println(result)
}
```

위의 예제 코드는 문자열 "hello world"를 대문자로 변환한 뒤 출력하는 간단한 코드입니다. Go 언어에서는 `strings` 패키지의 `ToUpper` 함수를 사용하여 문자열을 대문자로 변환할 수 있습니다. 이 외에도 `bytes` 패키지의 `ToUpper` 함수를 사용하거나, 해당 문자의 아스키 코드를 32만큼 빼는 방법으로 대소문자를 구분하지 않는 비교가 가능합니다.

## 깊게 들어가보기

대문자로 변환하는 과정에서 실제로 어떤 일이 일어나는지 알아보겠습니다. Go 언어에서 문자열은 바이트(BYTE) 형태로 저장됩니다. 즉, 문자 하나마다 고정된 바이트 수를 가지고 있으며, 이를 통해 문자의 아스키 코드를 확인할 수 있습니다. 대문자와 소문자의 아스키 코드 차이는 32입니다. 따라서 대문자로 변환하기 위해서는 문자마다 아스키 코드에 32를 빼주면 됩니다.

예를 들어, 문자 "h"의 아스키 코드는 104입니다. 이에 32를 빼주면 대문자 "H"의 아스키 코드인 72가 됩니다. 이것이 바로 Go 언어에서 대문자로 변환하는 과정입니다.

## 참고

- "strings" 패키지 공식 문서: https://golang.org/pkg/strings/
- "bytes" 패키지 공식 문서: https://golang.org/pkg/bytes/
- ASCII 코드표: https://ko.wikipedia.org/wiki/ASCII
- Go 언어 공식 문서: https://golang.org/doc/