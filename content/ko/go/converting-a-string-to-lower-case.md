---
title:                "문자열을 소문자로 변환하기"
html_title:           "Bash: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
문자열을 소문자로 변환하는 것은 문자의 대소문자를 일관성있게 관리하기 위한 프로그래밍 기술입니다. 이것은 대소문자 구분 없이 데이터 검색 및 정렬 등을 진행할 때 유용합니다.

## 어떻게 하나요?
밑의 Go 코드는 string을 소문자로 변환하는 방법을 보여줍니다:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "Hello, Gopher!"
	result := strings.ToLower(str)
	fmt.Println(result)
}
```
이 프로그램을 실행하면, 출력으로 "hello, gopher!"를 볼 수 있습니다.

## 깊은 곳으로: 
1. **역사적 맥락**: Go 언어의 `strings` 패키지는 문자열을 소문자로 변환하는 `ToLower` 함수를 제공합니다. 이 함수는 문자열의 각 문자를 해당하는 소문자로 바꾸는 역할을 합니다.

2. **대체 방법**: `ToLower` 함수 대신 Unicode point를 사용하여 소문자로 변환할 수도 있습니다. 하지만 일반적으로 `ToLower` 함수를 사용하는 것이 더 효율적입니다.

3. **구현 세부사항**: `ToLower` 함수는 Go 언어의 `unicode` 패키지를 사용하여 소문자로 변환을 수행합니다. 각 문자는 유니코드로 매핑되고, 해당 유니코드 값의 소문자가 출력됩니다.

## 참고 자료:
1. [Go 언어 공식 문서 – strings 패키지](https://golang.org/pkg/strings/)
2. [Go 프로그래밍에서 소문자 변환에 대한 토론](https://stackoverflow.com/questions/47341278/how-to-convert-a-string-to-lower-case-in-golang)