---
title:                "문자열의 길이 찾기"
date:                  2024-01-20T17:47:28.960947-07:00
model:                 gpt-4-1106-preview
simple_title:         "문자열의 길이 찾기"

category:             "Go"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
문자열 길이 찾기란 무엇인가? 문자열에 포함된 문자의 수를 셉니다. 프로그래머는 왜 이걸 하는 걸까요? 데이터 유효성 검사, 메모리 관리, 사용자 입력 처리 등을 위해서죠.

## How to:
```Go
package main

import (
	"fmt"
	"unicode/utf8"
)

func main() {
	str := "안녕하세요"
	fmt.Println("Bytes:", len(str))               // 바이트 길이
	fmt.Println("Runes:", utf8.RuneCountInString(str)) // 룬 길이

	// 출력:
	// Bytes: 15
	// Runes: 5
}
```

## Deep Dive
문자열의 길이를 찾는 건 복잡할 수 있습니다. 왜냐하면, Go 문자열은 UTF-8 인코딩된 바이트의 시퀀스이기 때문인데, 이는 모든 글자가 같은 바이트를 사용하지 않음을 의미합니다. 예를 들어, '안'과 '하'는 두 바이트 이상을 사용합니다. 고 자체의 `len` 함수는 바이트 수를 세지만, 문자 수를 정확히 셀 때는 `unicode/utf8` 패키지를 사용해야 합니다.

역사적으로 Go는 처음부터 UTF-8 인코딩을 지원했습니다, 이는 전 세계 여러 언어를 표현할 수 있도록 합니다. 룬(rune) 타입은 문자 하나를 나타내기 위한 int32의 별칭입니다. 이를 사용하면 각 문자에 접근하고 다룰 수 있죠.

문자열을 배열로 변환하여 길이를 찾는 다른 방법도 있습니다. `[]rune(str)`와 같이 문자열을 변환하면 개별 문자에 접근할 수 있으며, 배열의 길이로 문자 수를 얻을 수 있습니다. 그러나 이 방법은 기본적으로 모든 문자에 대한 복사본을 만드는, 덜 효율적인 과정입니다.

## See Also
- Go 공식 문서에서 문자열 처리에 대해 더 알아보기: [Go Doc - Strings](https://golang.org/pkg/strings/)
- Unicode를 이해하고 사용하는 방법: [The Unicode Consortium](http://unicode.org)
- UTF-8 인코딩과 룬에 대해 더 배우기: [Go Blog - Strings, bytes, runes and characters in Go](https://blog.golang.org/strings)
