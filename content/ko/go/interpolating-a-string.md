---
title:                "문자열 보간하기"
date:                  2024-01-20T17:51:04.212763-07:00
model:                 gpt-4-1106-preview
simple_title:         "문자열 보간하기"

category:             "Go"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜 사용하는가?)
문자열 보간(string interpolation)은 변수나 표현식의 값을 문자열 안에 집어넣는 방법입니다. 코드를 더 읽기 쉽고 관리하기 편하게 만들기 위해 사용합니다.

## How to: (방법)
Go에서 문자열 보간은 `fmt.Printf`, `fmt.Sprintf` 등을 이용하여 수행합니다. 살펴보죠.

```Go
package main

import "fmt"

func main() {
    name := "세종대왕"
    age := 26

    // Printf를 사용하여 콘솔에 출력
    fmt.Printf("안녕하세요, 제 이름은 %s이고 나이는 %d살입니다.\n", name, age)

    // Sprintf를 사용하여 문자열 변수에 저장
    greeting := fmt.Sprintf("안녕, %s! %d번째 생일 축하해!", name, age)
    fmt.Println(greeting)
}
```

실행 결과:
```
안녕하세요, 제 이름은 세종대왕이고 나이는 26살입니다.
안녕, 세종대왕! 26번째 생일 축하해!
```

## Deep Dive (심층 분석)
초기 프로그래밍 언어에선 문자열 보간이 없었습니다. 개발자들은 문자열을 연결(concatenation) 할 수밖에 없었죠. Go는 형식 지정자(format specifier)를 사용하여 문자열에 값을 삽입합니다. 예를 들어, `%s`는 문자열, `%d`는 정수를 나타냅니다.

보간 대안으로는 `+` 연산자를 사용한 문자열 연결이나, `strings` 패키지의 `Join` 함수가 있습니다. 그러나 보간은 코드의 명확성과 효율성 면에서 일반적으로 우수합니다.

내부적으로, `fmt` 패키지의 함수들은 I/O 작업과 메모리 할당을 처리하는데, 실제 보간 작업은 Go 런타임과 연관된 복잡한 과정을 거칩니다.

## See Also (더 보기)
- [Go by Example: String Formatting](https://gobyexample.com/string-formatting)
- [Go’s fmt package](https://golang.org/pkg/fmt/)
- [Go Blog: String handling](https://blog.golang.org/strings)
