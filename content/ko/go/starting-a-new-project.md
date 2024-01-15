---
title:                "새로운 프로젝트 시작하기"
html_title:           "Go: 새로운 프로젝트 시작하기"
simple_title:         "새로운 프로젝트 시작하기"
programming_language: "Go"
category:             "Go"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 왜

새 프로젝트를 시작하는 이유는 다양합니다. 가장 많이 드는 이유는 새로운 기술에 도전하거나 개발 스킬을 향상시키기 위해서입니다.

## 시작 방법

먼저 Go 언어를 다운로드하고 설치해야 합니다. 그리고 다음과 같이 `hello.go` 파일을 만들어서 코드를 작성합니다.

```Go
package main

import "fmt"

func main() {
  fmt.Println("Hello, world!")
}
```

이제 컴파일하고 실행해 봅시다.

```
$ go build hello.go
$ ./hello
Hello, world!
```

축하합니다! 당신은 첫 Go 프로그램을 만들었습니다.

## 딥 다이브

새 프로젝트를 시작하는 것은 가장 흥미로운 도전 중 하나입니다. 이것은 단순히 새로운 언어를 배우는 것 이상을 의미합니다. 당신은 다양한 도구와 기술을 배우고 혼자서도 문제를 해결할 수 있게 됩니다.

## 더 알아보기

- Go 공식 사이트: https://golang.org/
- Go 튜토리얼: https://tour.golang.org/welcome/1
- Go 언어 자습서: https://golang.org/doc/tutorial/
- Go 커뮤니티 포럼: https://forum.golangbridge.org/