---
title:                "Go: 텍스트 검색 및 대체하기"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜

텍스트 검색 및 치환의 필요성이 무엇인지 궁금하신가요? 이 블로그 포스트에서는 Go 언어를 사용하여 텍스트 검색 및 치환을 하는 방법과 그 과정에서 더 깊이 들여다볼 수 있는 정보를 제공합니다.

## 어떻게

텍스트 검색 및 치환을 수행하는 방법은 매우 간단합니다. 우선, 소스 코드에서 바꾸고자 하는 문자열을 찾은 다음, 지정한 새로운 문자열로 대체하면 됩니다. 이를 Go 언어의 `strings` 패키지를 사용하여 쉽게 구현할 수 있습니다.

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	// 원래 문자열
	str := "안녕하세요! 오늘은 출근일입니다."

	// "안녕하세요"를 "Hello"로 대체
	newStr := strings.Replace(str, "안녕하세요", "Hello", 1)

	// 결과 출력
	fmt.Println(newStr)
}
```

위의 예시 코드에서는 `strings.Replace()` 함수를 사용하여 텍스트를 대체하였습니다. 이 함수는 세 가지 파라미터를 입력받습니다. 첫 번째 파라미터는 원본 문자열이고, 두 번째 파라미터는 찾을 문자열이며, 세 번째 파라미터는 대체할 문자열입니다. 마지막으로, 원본 문자열에서 찾는 모든 문자열을 대체하고 싶다면 마지막 인자로 `-1`을 입력하시면 됩니다.

위의 코드를 실행하면 다음과 같은 결과가 출력됩니다.

```Go
Hello! 오늘은 출근일입니다.
```

## 깊이 들여다보기

텍스트 검색 및 치환 작업을 더 깊이 들여다보려면 다음과 같은 함수들을 사용할 수 있습니다.

- `strings.Contains()`: 문자열에 특정 문자열이 포함되어 있는지 여부를 확인합니다.
- `strings.Count()`: 특정 문자열이 문자열 내에서 몇 번 등장하는지 세어줍니다.
- `strings.Index()`: 문자열 내에서 특정 문자열이 처음으로 등장하는 인덱스를 반환합니다.

이 외에도 여러 가지 함수를 사용하여 보다 복잡한 텍스트 검색 및 치환 작업을 수행할 수 있습니다. 자세한 내용은 [Go 언어 공식 문서](https://golang.org/pkg/strings/)를 참고하시기 바랍니다.

## 같이 보기

- [Go 언어 공식 문서 (strings 패키지)](https://golang.org/pkg/strings/)
- [Go 언어로 파일 읽고 쓰기](https://medium.com/@ravinderpgupta/reading-file-in-golang-39ab81f85985)