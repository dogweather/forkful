---
title:                "문자열의 길이 찾기"
html_title:           "Go: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 무엇이며 왜?
문자열의 길이를 찾는 것은 프로그래머들이 자주 하는 작업 중 하나입니다. 문자열의 길이를 알면 문자열을 처리하는 데 매우 유용합니다.

## 방법:
문자열의 길이를 찾는 가장 간단한 방법은 ```len(str)``` 함수를 사용하는 것입니다. 이 함수는 문자열의 길이를 정수로 반환합니다.

예제:

```Go
str := "Hello World"
length := len(str)
fmt.Println(length) // Output: 11
```

위의 예제에서는 ```len(str)``` 함수를 사용하여 문자열 "Hello World"의 길이를 찾은 후, ```fmt.Println()``` 함수를 사용하여 결과를 출력하였습니다.

## 딥 다이브:
문자열의 길이를 찾는 기능은 Go언어에서 기본적으로 제공되는 기능입니다. 이는 문자열을 처리하는 프로그래밍 언어에서 핵심적인 기능 중 하나이기도 합니다. 이러한 기능은 C언어에서 영향을 받았는데, C언어에서는 문자열의 끝을 나타내기 위해 널 문자를 사용하기 때문입니다.

또한, Go언어에서는 문자열의 길이를 찾는 다른 방법으로 ```range```를 사용하는 것이 가능합니다. 이 방법은 문자열의 각 문자를 반복하면서 문자열의 길이를 카운트하는 방식입니다. 하지만 이 방법은 추천되지 않으며, 문자열의 길이를 찾는 가장 효율적인 방법은 ```len()``` 함수를 사용하는 것입니다.

## 관련 자료:
- [Go언어 공식 문서](https://golang.org/doc/)
- [Go언어 튜토리얼](https://www.tutorialspoint.com/go/index.htm)
- [Go언어 블로그](https://blog.golang.org/)