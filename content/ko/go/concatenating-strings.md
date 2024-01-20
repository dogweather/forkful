---
title:                "문자열 연결하기"
html_title:           "Arduino: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## 무엇이고 왜합니까?
문자열 연결은 여러 문자열을 하나로 결합하는 과정입니다. 이것은 프로그래머가 복수의 문자열 조각을 하나의 문자열로 결합해야 하는 경우, 가령 UI 메시지, 로그 메시지, 에러 메시지 생성 등등에서 필요합니다.

## 어떻게 하는가:
Go 언어에서는 다음과 같은 몇가지 방법으로 문자열 연결을 수행할 수 있습니다.

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	// + 기호를 사용해 문자열 연결
	str1 := "안녕하세요, "
	str2 := "Go 프로그래밍을 배우는 중입니다."
	result := str1 + str2
	fmt.Println(result) // 결과: 안녕하세요, Go 프로그래밍을 배우는 중입니다.

	// `fmt.Sprintf` 함수를 사용해 문자열 연결
	result2 := fmt.Sprintf("%s%s", str1, str2)
	fmt.Println(result2) // 결과: 안녕하세요, Go 프로그래밍을 배우는 중입니다.

	// `strings.Join` 함수를 사용해 문자열 연결
	strSlice := []string{"안녕하세요, ", "Go 프로그래밍을 배우는 중입니다."}
	result3 := strings.Join(strSlice, "")
	fmt.Println(result3) // 결과: 안녕하세요, Go 프로그래밍을 배우는 중입니다.
}
```

## 깊게 알아보기
문자열 연결은 거의 모든 프로그래밍 언어에서 보편적인 작업으로, 이 작업의 효과적인 실행을 위해 여러 가지 방법이 제안되었습니다. Go 언어에서는 세 가지 주요 방법이 제시되었고, 그 중 마지막 두 가지 방법이 특히 성능 저하 없이 대량의 문자열을 연결하는 데 효과적입니다.

`fmt.Sprintf` 함수가 제공하는 편리성과 `strings.Join` 함수의 성능은 이들을 많이 사용되게 만듭니다. 그러나 이 중 어느 것을 사용할지는 당신의 상황에 따라 달라집니다. 어떤 경우에는 단순히 `+` 연산자가 가장 명확하고 간결할 수 있습니다.

## 참고 링크
1. Go 언어에서 문자열 처리에 대한 자세한 내용: [Go 공식 문서](https://golang.org/pkg/strings/)
2. 문자열 연결에 대한 효율성에 대한 자세한 내용: [Go 언어 블로그](https://go.dev/blog/strings)