---
title:                "문자열 연결하기"
html_title:           "Go: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열 연결이 무엇인지에 대해서는 두 가지 정도의 문장으로 설명할 수 있습니다. 첫 번째로, 문자열을 연결하는 것은 두 개 이상의 문자열을 하나의 문자열로 합치는 것을 말합니다. 두 번째로, 프로그래머들은 문자열 연결을 하는 이유는 보다 복잡한 문자열을 만들기 위해서입니다.

## 어떻게:

```Go
str1 := "Hello,"
str2 := " world!"
result := str1 + str2
fmt.Println(result)
```

출력: Hello, world!

위의 예시 코드에서는 두 개의 문자열을 하나의 변수에 할당하고, 이를 다시 새로운 변수에 연결하여 출력하는 방식을 보여줍니다. 또한, 문자열을 직접 연결하는 것 외에도, `fmt.Sprintf` 함수를 사용하여 포맷을 지정하여 문자열을 연결할 수도 있습니다.

## 깊이 들어가보기:

문자열 연결이 사용되는 상황은 다양합니다. 예를 들어, 사용자 이름과 이메일 주소를 함께 표시해야 할 때, 문자열 연결을 통해 간단하게 표현할 수 있습니다. 이 방식은 많은 프로그래밍 언어들에서 지원되며, Go에서도 표준 라이브러리인 `strings` 패키지에 `Join` 함수를 통해 문자열 연결을 할 수 있습니다.

또한, Go에서는 `bytes.Buffer` 타입을 통해 문자열 연결을 할 수도 있습니다. 이 방식은 많은 연결 작업이 필요한 경우, 성능 측면에서 더 효율적이며, 표준 라이브러리에서도 주로 이 타입을 사용하고 있습니다.

## 관련 자료:

- [Go strings 패키지 문서](https://golang.org/pkg/strings/)
- [Effective Go - 문자열 연결](https://golang.org/doc/effective_go.html#string_concat)
- [Go 디자인 문서 - 문자열 연산](https://golang.org/doc/go1.11#string_operators)