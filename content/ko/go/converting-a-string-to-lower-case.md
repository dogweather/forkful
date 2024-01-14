---
title:    "Go: 문자열 소문자로 변환하기"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

본격적으로 코딩을 하기 전에 가장 많이 실수하는 것 중 하나가 입력 받은 문자열의 대소문자를 구분하지 않아서 발생하는 문제입니다. 이러한 실수를 방지하기 위해서는 문자열을 모두 소문자로 변환하는 과정이 필요합니다.

## 왜 변환해야 하는가?

소문자로 변환할 경우 문자열을 비교할 때 대소문자를 구분하지 않아도 되기 때문에, 입력 받은 값과 프로그램에서 사용하는 값을 정확하게 비교할 수 있습니다. 또한, 대부분의 데이터베이스나 파일 시스템은 대소문자를 구분하지 않기 때문에, 일관성 있는 데이터를 처리하기 위해서도 필요합니다.

## 변환하는 방법

Go 언어에서는 strings 패키지에서 strings.ToLower() 함수를 사용하여 문자열을 소문자로 변환할 수 있습니다. 아래의 예시를 참고해보세요.

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	word := "CoNvErT"
	fmt.Println("입력된 문자열:", word)
	fmt.Println("대소문자 변환 결과:", strings.ToLower(word))
}
```

출력 결과는 다음과 같습니다.

```bash
입력된 문자열: CoNvErT
대소문자 변환 결과: convert
```

## 깊이있는 정보

Go 언어에서는 문자열을 변경할 수 없는 immutable data type으로 정의되어 있습니다. 따라서, strings.ToLower() 함수는 문자열을 새로운 메모리 공간에 할당하여 변환된 값을 반환하는 것입니다. 또한, ASCII 문자만 변환되고, 유니코드 문자는 변환되지 않는 것을 알 수 있습니다.

## 같이 보기

- [Go 언어 공식 문서 - strings 패키지](https://golang.org/pkg/strings/)
- [Golang Cafe - 문자열 변환](https://www.golang.cafe/blog/golang-string-functions.html#converting-string-case)
- [코딩집중력 블로그 - Go 언어 문자열 처리](https://coding-artist.tistory.com/11)