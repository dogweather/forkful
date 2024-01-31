---
title:                "문자열 대문자로 변환하기"
date:                  2024-01-19
html_title:           "Arduino: 문자열 대문자로 변환하기"
simple_title:         "문자열 대문자로 변환하기"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가 & 왜?)
문자열을 대문자화하는 것은 모든 문자를 대문자로 변환하는 것입니다. 가독성을 높이고, 텍스트의 통일성을 보장하기 위해 프로그래머들은 이를 사용합니다.

## How to: (하는 방법)
Go에서 문자열을 대문자로 만들려면 `"strings"` 표준 라이브러리의 `ToUpper` 함수를 사용합니다. 아래는 간단한 예제입니다:

```go
package main

import (
    "fmt"
    "strings"
)

func main() {
    originalString := "hello, world!"
    upperString := strings.ToUpper(originalString)
    fmt.Println(upperString) // 출력: HELLO, WORLD!
}
```

## Deep Dive (심층 붾석)
문자열 대문자화는 ASCII 시대부터 이용됐습니다. 특히 초기 컴퓨터 시스템은 대소문자 구분이 없었죠. `ToUpper`는 간단해 보이지만, 내부적으로는 각 문자를 ASCII 또는 유니코드 테이블에서 대응하는 대문자로 매핑합니다. Unicode가 등장하며 다국어 지원이 중요해졌는데, `ToUpper`는 다양한 언어 및 문자 시스템을 고려해 대문자를 만듭니다.

대안으로, 직접 루프를 돌며 각 문자를 대문자로 변경할 수도 있지만, 이는 번거로우며 `ToUpper`가 이미 잘 최적화되어 있습니다. 유의할 점은 `ToUpper` 함수가 새로운 문자열을 반환한다는 것인데, Go의 문자열은 불변이기 때문입니다.

## See Also (참고 자료)
- Go Doc for strings.ToUpper: https://pkg.go.dev/strings#ToUpper
- Unicode Standard: https://unicode.org/standard/standard.html
- Go Blog about Strings: https://blog.golang.org/strings
