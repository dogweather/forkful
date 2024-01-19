---
title:                "패턴에 일치하는 문자 삭제"
html_title:           "Fish Shell: 패턴에 일치하는 문자 삭제"
simple_title:         "패턴에 일치하는 문자 삭제"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
패턴과 일치하는 문자를 삭제하는 것은 특정 룰에 따라 문자열에서 문자를 제거하는 프로그래밍 기술입니다. 이것은 불필요한 공백, 특수기호, 불필요한 텍스트를 제거함으로써 데이터를 정확하고 깔끔하게 만드는 데 도움이 됩니다.

## 어떻게 하는가:
Go에서 패턴과 일치하는 문자를 제거하는 방법은 `strings.Replace` 함수를 사용하는 것입니다. 아래 코드 예제를 살펴보십시오:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	text := "안-녕-하-세-요! 고-롱!"
	cleanText := strings.Replace(text, "-", "", -1)

	fmt.Println(cleanText)
}
```

이 예제에서는 모든 하이픈('-')을 공백으로 치환했습니다. 코드를 실행하면 아래 출력이 반환됩니다:

```Go
안녕하세요! 고롱!
```

## 깊게 알아보기:
문자를 패턴과 일치하여 삭제하는 기능은 Unix의 tr 명령어에서 시작되었습니다. 이제는 모든 현대적인 프로그래밍 언어가 이 기능을 지원합니다. 

Go언어에서는 `strings.Replace` 이외에도 `strings.Trim`, `strings.ReplaceAll` 등의 함수가 제공되어 문자열에서 필요한 문자를 걸러낼 수 있습니다.

내부적으로는 Go 언어가 각 문자를 검사하여 일치하는 것이 있는지 체크하고, 일치하는 문자가 없는 새로운 문자열을 생성합니다. 이로 인해 문자열을 조작하는 데 자원을 소모하지만, 대부분의 경우 이는 무시할 수 있는 수준입니다.

## 참고 자료:
- [Go: strings 패키지 공식 문서](https://golang.org/pkg/strings/)
- [Go 로 시작하는 문자열 처리](https://gobyexample.com/strings)
- [Go 튜토리얼: 문자열](https://www.tutorialspoint.com/go/go_strings.htm)