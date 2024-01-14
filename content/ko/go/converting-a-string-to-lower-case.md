---
title:                "Go: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜

문자열을 소문자로 변환하는 것의 중요성을 알아보겠습니다. Go 프로그래밍 언어에서 문자열을 소문자로 변환하는 방법은 코드 작성과 문제 해결에 매우 유용합니다.

## 하는 법

```Go
// 예제 1: strings 패키지 사용하기
package main

import (
  "fmt"
  "strings"
)

func main() {
  str := "Go 언어와 함께하는 프로그래밍은 즐겁습니다!"
  fmt.Println("소문자로 변환하기 전:", str)
  str = strings.ToLower(str)
  fmt.Println("소문자로 변환한 후:", str)
}
```

<출력 결과>
소문자로 변환하기 전: Go 언어와 함께하는 프로그래밍은 즐겁습니다!
소문자로 변환한 후: go 언어와 함께하는 프로그래밍은 즐겁습니다!

```Go
// 예제 2: loop를 사용하여 각 문자를 소문자로 변환하기
package main

import "fmt"

func main() {
  str := "GOLANG"
  fmt.Println("소문자로 변환하기 전:", str)
  res := ""
  for _, v := range str {
    if v >= 'A' && v <= 'Z' {
      res = fmt.Sprintf("%s%c", res, v+32)
    } else {
      res = fmt.Sprintf("%s%c", res, v)
    }
  }
  fmt.Println("소문자로 변환한 후:", res)
}
```

<출력 결과>
소문자로 변환하기 전: GOLANG
소문자로 변환한 후: golang

이러한 방식으로 문자열을 소문자로 변환할 수 있습니다. 하지만 이러한 방식은 문자열이 긴 경우 시간이 오래 걸리고 코드가 복잡해질 수 있으므로 문자열을 소문자로 변환하는 더 좋은 방법이 있습니다.

## 딥 다이브

Go 언어의 strings 패키지에는 ToLower 함수가 있습니다. 이 함수는 문자열을 소문자로 변환하여 새로운 문자열을 반환합니다. 또한, 알파벳 외의 문자는 그대로 유지됩니다. 이 함수는 매우 간단하며 효율적이기 때문에 이 방법을 사용하는 것이 좋습니다.

하지만 만약 전체 문자열을 소문자로 변환하는 것이 아니라 각 문자를 소문자로 변환하고 싶다면, strings 패키지의 ToLower 함수를 for loop와 함께 사용할 수 있습니다. 이 방법은 각 문자를 탐색하며 문자가 대문자일 경우 소문자로 변환하여 새로운 문자열에 추가합니다.

## 관련 링크

* [Go 언어의 strings 패키지 문서](https://golang.org/pkg/strings/#ToLower)
* [Go 언어문서에서 제공하는 문자열 함수 목록](https://golang.org/pkg/strings/)