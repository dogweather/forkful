---
title:                "Go: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열의 길이를 알아내는 것에 대해 궁금해 할 수 있습니다. 이것은 프로그래밍에서 매우 중요한 개념이며, 문자열 데이터를 처리할 때 도움이 될 수 있기 때문입니다.

## 하는 법

가장 간단한 방법은 내장된 `len()` 함수를 사용하는 것입니다. 이 함수는 문자열의 길이를 정수 형태로 반환합니다. 예를 들어, 다음과 같이 코드를 작성할 수 있습니다.

```Go
string := "안녕하세요"
length := len(string)
fmt.Println(length)
```

이 코드의 출력은 `5`가 됩니다. 왜냐하면 "안녕하세요"라는 문자열은 다섯 개의 문자로 이루어져 있기 때문입니다.

이것 외에도 `for` 루프나 문자열의 인덱스를 이용하여 각 문자를 반복하면서 개수를 세는 방법도 있습니다. 하지만 이러한 방법은 좀 더 복잡하고 번거롭기 때문에 평소에는 내장된 `len()` 함수를 사용하는 것이 더 편리합니다.

## 깊이 파고들기

문자열의 길이를 알아내는 것은 컴퓨터의 메모리와 밀접한 관련이 있습니다. 문자열의 각 문자는 메모리 상에 위치하며, 해당 문자열의 길이는 메모리에서 차지하는 공간의 크기와 동일합니다.

또한 문자열의 길이는 여러 가지 다른 인코딩 방식에 따라 달라지며, 따라서 문자열을 다룰 때 이러한 인코딩 방식을 고려해야 합니다. 예를 들어, UTF-8 인코딩 방식을 사용하면 한글문자 하나가 여러 바이트로 구성되는 경우가 있으므로, 문자열의 길이를 알아낼 때 이점을 유의해야 합니다.

## 참고 자료

- [Go 프로그래밍 언어 공식 문서 - 문자열](https://golang.org/ref/spec#String_types)
- [코드 아카데미 - Go 언어 강좌](https://www.codecademy.com/courses/learn-go)