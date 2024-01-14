---
title:                "Go: 패턴과 일치하는 문자 삭제"
simple_title:         "패턴과 일치하는 문자 삭제"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# 왜: 

누군가가 패턴과 일치하는 문자를 삭제하는 것에 참여하는 이유는 무엇일까요? 즉시 *왜* 하는지에 대한 1-2개 문장의 설명입니다.

## 어떻게:

```Go
package main

import "fmt"
import "regexp"

func main() {
	// 입력 문자열
	str := "Hello, 안녕하세요!"

	// 패턴을 컴파일하여 정규식 객체 만들기
	pattern := regexp.MustCompile("[^a-zA-Z ]+")

	// 패턴과 일치하는 모든 문자 삭제하기
	output := pattern.ReplaceAllString(str, "")
	
	// 결과 출력
	fmt.Println(output) // "Hello"
}
```

위의 코드 예제에서는 입력으로 "Hello, 안녕하세요!"라는 문자열을 받고, 패턴으로는 알파벳과 공백이 아닌 모든 문자를 공백으로 변환하여 삭제합니다. 결과로는 "Hello"가 출력됩니다.

## 딥 다이브:

패턴과 일치하는 문자를 삭제하는 기능은 문자열 처리에서 매우 중요합니다. 정규식은 이를 더 쉽게 만들어주는 강력한 도구입니다. 더 많은 패턴 매칭 기법을 익히기 위해서는 [Go regexp 패키지](https://golang.org/pkg/regexp/) 문서를 참고하시기 바랍니다.

# 더 알아보기:

- [Go 언어의 정규식 사용하기](https://gobyexample.com/regular-expressions)
- [Go 문자열 함수 및 정규식 사용하기](https://www.callicoder.com/golang-strings-usage/)