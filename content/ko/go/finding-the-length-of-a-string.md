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

## 왜

문자열 길이를 찾는 방법을 배우는 이유는 프로그래밍에서 굉장히 일반적이기 때문입니다. 문자열은 컴퓨터 프로그램에서 매우 중요한 역할을 하며, 그 길이를 찾는 것은 이를 효율적으로 다룰 수 있는 필수적인 기술입니다.

## 어떻게 하나요

```
Go package main

import (
	"fmt"
)

func main() {
	// 문자열 변수 선언 및 초기화
	myString := "안녕하세요!"

	// 문자열 길이를 반환하는 내장 함수 len()을 사용하여 길이 찾기
	length := len(myString)

	// 결과 출력
	fmt.Println("문자열의 길이:", length)
}
```
**결과: 문자열의 길이: 7**

위의 예시 코드를 통해 Go에서 문자열의 길이를 찾는 방법을 쉽게 학습할 수 있습니다. 문자열 변수를 선언하고 문자열 길이를 반환하는 내장 함수를 사용하여 원하는 결과를 출력할 수 있습니다.

## 깊이있는 탐구

Go에서는 문자열의 길이를 찾기 위해 내장 함수인 `len()`을 사용합니다. 이 함수는 실제로 문자열의 길이를 계산하는 것이 아니라 메모리에서 해당 문자열을 찾아 그 길이를 반환합니다. 때문에, 해당 문자열이 UTF-8 인코딩을 따르지 않는다면 실제 길이와 다른 결과가 나올 수 있습니다. 또한, 문자열에는 숫자, 알파벳 뿐만 아니라 유니코드 문자까지 모두 포함되므로, 이를 고려하여 길이를 계산해야 합니다. 따라서 Go에서 문자열 길이를 찾을 때는 UTF-8 인코딩과 유니코드 문자를 고려해야 한다는 점을 알고 있어야 합니다.

## 더 알아보기

- Go 공식 문서: https://golang.org/doc/
- Learn Go in Y minutes: https://learnxinyminutes.com/docs/go/
- UTF-8 인코딩에 대한 자세한 설명: https://ko.wikipedia.org/wiki/UTF-8
- 유니코드에 대한 개요: https://en.wikipedia.org/wiki/Unicode