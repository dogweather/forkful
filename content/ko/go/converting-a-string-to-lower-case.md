---
title:                "문자열을 소문자로 변환하기"
html_title:           "Go: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜
이번 글에서는 Go 언어를 사용하여 문자열을 소문자로 변환하는 방법에 대해 알아보겠습니다. 문자열을 소문자로 변환하는 것은 다양한 이유로 많은 사람들이 필요로 하는 기능입니다. 예를 들어, 사용자의 입력을 정리하기 위해 소문자로 변환하거나, 문자열 비교를 할 때 대소문자를 구분하지 않기 위해 소문자로 변환하는 경우가 있습니다.

## 방법
해당 기능을 수행하기 위해 ```strings``` 패키지의 ```ToLower()``` 함수를 사용할 수 있습니다. 이 함수는 하나의 매개변수로 문자열을 받고, 모든 문자를 소문자로 변환한 뒤 새로운 문자열로 반환합니다. 아래의 예제를 보면서 실제로 어떻게 사용되는지 살펴보겠습니다.

```Go
// 문자열을 소문자로 변환하는 예제 코드
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "HeLLo WoRld"
    lowerStr := strings.ToLower(str)
    fmt.Println(lowerStr)
}

// 출력 결과
hello world
```

위의 예제 코드에서 ```ToLower()``` 함수를 사용하여 문자열을 소문자로 변환하고, 변환된 결과를 새로운 변수에 저장한 뒤 출력해주는 부분을 살펴보세요. 이처럼 간단하게 사용할 수 있어서 매우 편리합니다.

## 딥 다이브
Go 언어에서는 문자열을 다루는 다양한 함수들을 제공하고 있습니다. 위에서 소개한 ```ToLower()``` 함수 외에도 ```ToUpper()```, ```Trim()```, ```Split()``` 등 다양한 함수를 사용하여 문자열을 다룰 수 있습니다. 따라서 해당 기능을 활용할 때는 이러한 함수들도 함께 알아두면 더욱 효율적으로 코드를 작성할 수 있습니다.

## 참고 자료
- [Go 언어 공식 문서](https://golang.org/pkg/strings/)
- [Effective Go - Strings](https://golang.org/doc/effective_go#strings)
- [The Go Blog - Strings, bytes, runes and characters in Go](https://blog.golang.org/strings)

## 더보기
더 다양한 Go 언어 관련 정보를 알고 싶다면 아래의 링크를 참고해보세요.
- [Go 언어 소개 및 설치 방법](https://blog.naver.com/PostView.nhn?blogId=gophers&logNo=220748538021)
- [Go 언어 기초 문법 정리](https://blog.naver.com/PostView.nhn?blogId=gophers&logNo=220823091956)
- [Go 언어를 활용한 웹 개발](https://blog.naver.com/PostView.nhn?blogId=gophers&logNo=221015325178)