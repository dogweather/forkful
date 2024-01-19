---
title:                "부분 문자열 추출"
html_title:           "Arduino: 부분 문자열 추출"
simple_title:         "부분 문자열 추출"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열 추출은 일련의 문자에서 원하는 부분 문자열을 가져오는 작업입니다. 프로그래머들이 데이터를 다룰 때 이를 이용해 필요한 정보만을 추려낼 수 있습니다.

## 어떻게:

Go에서 문자열 추출은 간단합니다. 인덱스를 이용해 원하는 부분을 가져오면 됩니다.

```Go
package main
import "fmt"

func main() {
	str := "안녕하세요. Go 프로그래밍입니다."
	fmt.Println(str[0:9])     // 출력: "안녕하세요."
	fmt.Println(str[10:26])    // 출력: "Go 프로그래밍입니다."
}
```

## 깊이 들어가보기: 

문자열 추출은 고대 프로그래밍 부터 존재했던 기능입니다. 덕분에 코드를 이해하고 디버그하는 데 도움이 될 뿐만 아니라, 정보를 처리 및 분석하는데 필수적입니다.

Go에서는 슬라이싱을 이용해 문자열을 추출합니다. 이 방법은 문자열을 바이트 배열로 처리하며, 각 인덱스는 해당 바이트를 참조합니다. 이로 인해 유니코드 문자의 추출에 주의해야 합니다.

## 참고:

- Go언어 공식 문자열 처리 가이드: https://blog.golang.org/strings
- Go언어 슬라이스 사용법: https://go.dev/blog/slices