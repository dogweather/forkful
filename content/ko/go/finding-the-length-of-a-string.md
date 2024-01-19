---
title:                "문자열의 길이 찾기"
html_title:           "Lua: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 무엇이며 왜인가?

문자열의 길이를 찾는 것은 문자열에 포함된 문자의 수를 측정하는 것입니다. 이는 배열, 리스트, 문자열 층위의 구조를 다루거나, 문자나 부분 문자열 중 일부를 찾거나 제거하려는 프로그래머에게 중요합니다.

## 어떻게 할까?

Go에서 문자열의 길이를 찾는 데는 내장 함수 "len"을 사용합니다. 아래는 간단한 예제 코드입니다:

```Go
package main

import "fmt"

func main() {
    str := "안녕하세요"
    fmt.Printf("문자열의 길이: %d", len(str))
}
```
위 코드를 실행하면 "문자열의 길이: 15"라는 결과를 얻을 수 있습니다.

## 깊은 이해

Go언어의 문자열 길이 측정 방법이 다른 언어와 다른 이유는, Go가 문자열을 UTF-8로 처리하기 때문입니다. 이것은 Go언어가 모든 유니코드 문자를 올바르게 처리할 수 있음을 의미합니다.

그러나 이런 처리는 문자열 속 각 글자가 다른 바이트 수를 가질 수 있음을 포함하며, 이는 len 함수에서 반환되는 "길이"가 기대하는 것과 항상 일치하지 않을 수 있음을 의미합니다.

길이를 찾는 대안적인 방법으로는 `range` 키워드와 `for` 루프를 사용하는 것이 있습니다. 이는 각 유니코드 '문자'의 수를 세는 데 사용할 수 있습니다:

```Go
package main

import "fmt"

func main() {
    str := "안녕하세요"
    count := 0
    for range str {
        count++
    }
    fmt.Printf("문자의 수: %d", count)
}
```
이 코드를 실행하면 "문자의 수: 5"라는 결과를 얻을 수 있습니다, 더 직관적인 정수를 제공합니다.

## 관련 자료

- Go 언어의 스트링 처리에 대한 자세한 정보: [여기](https://blog.golang.org/strings)
- Go에서 유니코드를 다루는 방법에 대한 자세한 정보: [여기](https://blog.golang.org/normalization)