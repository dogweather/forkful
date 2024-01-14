---
title:                "Go: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜

문자열을 연결하는 이유는 프로그래밍에서 매우 일반적인 작업입니다. 문자열을 연결하면 변수, 함수 또는 조건 등을 조합하여 원하는 결과를 얻을 수 있습니다. 이것은 Go 프로그래밍에서도 똑같이 적용됩니다. 문제를 해결하기 위해 문자열을 연결하는 방법을 알아보겠습니다.

## 어떻게

Go 언어에서는 문자열을 연결하기 위해 `+` 연산자가 사용됩니다. 두 개의 문자열을 이어붙이는 간단한 예제를 살펴보겠습니다.

먼저, `hello`와 `world`라는 두 개의 변수를 선언하고 값을 할당합니다. 그런 다음, `+` 연산자를 사용하여 두 변수를 연결하고 그 결과를 출력합니다.

```Go
hello := "hello"
world := "world"
fmt.Println(hello + world)
```

위의 코드를 실행하면 `helloworld`라는 결과가 출력됩니다. 이처럼 `+` 연산자를 사용하면 여러 개의 문자열을 쉽게 연결할 수 있습니다.

또한, Go 언어에서는 `fmt.Sprintf()` 함수를 사용하여 여러 개의 변수를 하나의 문자열로 연결할 수도 있습니다. 아래의 예제를 확인해보세요.

```Go
age := 28
name := "John Doe"
greeting := fmt.Sprintf("Hello, my name is %s and I am %d years old.", name, age)
fmt.Println(greeting)
```

위 코드를 실행하면 `Hello, my name is John Doe and I am 28 years old.`라는 결과가 출력됩니다.

## 깊이 들어가기

Go 언어에서는 문자열을 연결할 때 메모리 할당이 발생하지 않습니다. 이는 문자열을 수정하는 메모리 블록을 새로 할당하는 것보다 효율적이기 때문입니다. 따라서, 문자열을 연결하는 것은 메모리 관리 측면에서 더 효율적이라고 할 수 있습니다.

또한, `+` 연산자와 `fmt.Sprintf()` 함수 이외에도 `strings.Join()` 함수를 사용하여 문자열을 더 효율적으로 연결할 수 있습니다. 이 함수는 문자열 슬라이스를 하나의 문자열로 연결해주는 기능을 합니다.

```Go
fruits := []string{"apple", "banana", "orange"}
joinedFruits := strings.Join(fruits, ", ")
fmt.Println(joinedFruits)
```

위의 코드를 실행하면 `apple, banana, orange`라는 결과가 출력됩니다.

## 더보기

- [A Tour of Go](https://go-tour-kr.appspot.com/basics/1): 공식 Go 투어 웹사이트로서, Go 언어의 기초부터 고급 개념까지 다양한 예제를 제공합니다.
- [Learning Go by Examples](https://github.com/golangbyexample/examples): Go 언어를 예제를 통해 학습할 수 있는 GitHub 저장소입니다.
- [Go Web Programming](https://github.com/PacktPublishing/Distributed-Systems-with-Go): Go를 이용하여 분산 시스템을 구축하는 방법을 학습할 수 있는 인기 있는 책입니다.

## 더보기