---
title:                "텍스트 검색 및 교체"
html_title:           "Go: 텍스트 검색 및 교체"
simple_title:         "텍스트 검색 및 교체"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

텍스트 검색 및 교체란 무엇인지 알아보고, 프로그래머가 왜 이를 하는지에 대해 이해합니다. 텍스트 검색 및 교체는 프로그래머가 코드나 파일 내에 특정한 텍스트를 찾아 바꾸는 작업을 의미합니다. 이를 통해 코드의 오류를 수정하고, 긴 코드를 더 간단하게 만들거나, 특정한 패턴을 찾아내는 등 다양한 목적을 달성할 수 있습니다.

## 방법:

```Go
// 문자열 내에서 특정한 단어를 찾아 다른 단어로 대체하는 예제
package main

import (
	"fmt"
	"strings"
)

func main() {
	text := "Hello, world!"
	replacedText := strings.ReplaceAll(text, "Hello", "Hi")
	fmt.Println(replacedText) // 출력결과: Hi, world!
}
```

```Go
// 배열 내에서 특정한 값을 찾아 다른 값으로 대체하는 예제
package main

import (
	"fmt"
)

func main() {
	numbers := []int{1,2,3,4,5}
	replacedNumbers := ReplaceAll(numbers, 3, 10)
	fmt.Println(replacedNumbers) // 출력결과: [1 2 10 4 5] 
}

func ReplaceAll(numbers []int, oldValue int, newValue int) []int {
	for index, value := range numbers {
		if value == oldValue {
			numbers[index] = newValue
		}
	}
	return numbers
}
```

## 깊이 들어가기:

텍스트 검색 및 교체 기능은 오래전부터 프로그래밍 언어에서 등장했습니다. 이 기능을 사용하면 코드 내에 있는 특정한 패턴을 찾아내고 이를 변경하여 코드의 의미를 바꿀 수 있습니다. Go 언어에서는 `strings` 라이브러리의 `ReplaceAll()` 함수를 사용하여 텍스트를 대체할 수 있습니다. 또한, `bytes` 라이브러리의 `Replace()` 함수를 사용하면 바이트 배열 내에서도 특정한 패턴을 찾아 다른 패턴으로 대체할 수 있습니다.

## 더 보기:

- [Go Documentation] (https://golang.org/pkg/strings/#ReplaceAll)
- [A Tour of Go: String Functions] (https://tour.golang.org/basics/5)
- [Go Tutorial: Arrays and Slices] (https://www.callicoder.com/golang-arrays-slices-guide/)