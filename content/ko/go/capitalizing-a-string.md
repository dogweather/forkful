---
title:                "문자열 대문자로 변환하기"
html_title:           "Go: 문자열 대문자로 변환하기"
simple_title:         "문자열 대문자로 변환하기"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 무엇이고 왜?
문자열을 대문자로 변환하는 것을 캐피털라이징이라고 합니다. 프로그래머들이 이를 하는 이유는 주로 문자열을 비교하기 쉽게 만들기 위해서입니다. 대문자와 소문자는 프로그래밍에서 다른 문자로 취급되기 때문에, 대문자로 변환된 문자열끼리 비교하면 더 정확한 결과를 얻을 수 있습니다.

## 어떻게:
Go 언어에서 캐피털라이징을 하는 방법은 간단합니다. 우선 `strings` 패키지를 임포트해야 합니다. 그 다음 `ToUpper()` 함수를 사용하여 문자열을 대문자로 변환할 수 있습니다. 간단한 예제를 보여드리겠습니다:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	name := "john smith"
	capitalized := strings.ToUpper(name)
	fmt.Println(capitalized)
}
```
**결과:** `JOHN SMITH`

또한, `Title()` 함수를 사용하면 문자열에서 각 단어의 첫 글자만 대문자로 변환할 수도 있습니다. 다음 예제를 확인해보세요:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	title := "the lord of the rings"
	capitalized := strings.Title(title)
	fmt.Println(capitalized)
}
```
**결과:** `The Lord Of The Rings`

## 딥 다이브:
캐피털라이징은 문자열을 비교하기 쉽게 만들기 위해 생겨난 개념입니다. 예전에는 대소문자를 구분하지 않는 언어들도 있었지만, 현재는 대부분의 프로그래밍 언어에서 대소문자를 구분합니다. 캐피털라이징은 이러한 구분을 쉽게 할 수 있도록 해줍니다. 또한, 문자열을 보기 좋게 정렬하거나 비교할 때도 유용합니다.

대부분의 언어에서는 캐피털라이징을 위해 `ToUpper()` 또는 `Title()` 함수를 제공합니다. 하지만 `strings` 패키지 이외에도 다른 패키지에서도 이와 비슷한 함수들을 제공할 수 있습니다. 어떤 패키지를 사용하든 결국은 문자열을 대문자로 변환하는 일은 같기 때문에, 어떤 함수를 사용할지는 프로그래머의 개인적인 취향에 따라 다를 수 있습니다.

## 관련 링크:
https://gobyexample.com/uppercase-strings - 캐피털라이징의 간단한 예제와 설명을 제공하는 곳입니다.

https://golang.org/pkg/strings/#ToUpper - Go 공식 문서에서 `ToUpper()` 함수의 사용법과 설명을 확인할 수 있습니다.