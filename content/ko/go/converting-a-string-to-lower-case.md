---
title:                "문자열을 소문자로 변환하기"
date:                  2024-01-20T17:38:25.699449-07:00
model:                 gpt-4-1106-preview
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜?)
문자열을 소문자로 변환하는 것은, 문자의 대소문자를 구분하지 않을 때 유용하다. 검색, 정렬, 데이터 일관성을 위해 종종 필요하다.

## How to: (방법)
Go에서 문자열을 소문자로 바꾸는 방법은 strings 패키지의 ToLower 함수를 사용하는 것입니다.

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	original := "Hello, World!"
	lowercased := strings.ToLower(original)
	fmt.Println(lowercased) // "hello, world!"
}
```

## Deep Dive (심도 있는 정보)
문자열을 소문자로 변환하는 것은 문자 인코딩과 closely related다. Go는 UTF-8 인코딩을 사용하며, 이는 국제화를 위한 표준이다. ToLower 함수는 Unicode 표준을 따르고 있다. 대안으로, bytes 패키지는 비슷한 기능을 제공하지만 byte slice에 대해서 작동한다. 구현 면에서, ToLower 함수는 각 문자를 확인하고 대응하는 소문자가 있으면 그것으로 변환한다.

## See Also (추가 자료)
- Go strings 패키지 문서: https://pkg.go.dev/strings
- Unicode 표준: https://www.unicode.org/standard/standard.html
- Go bytes 패키지 문서: https://pkg.go.dev/bytes
