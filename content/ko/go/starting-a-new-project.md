---
title:                "새 프로젝트 시작하기"
html_title:           "Arduino: 새 프로젝트 시작하기"
simple_title:         "새 프로젝트 시작하기"
programming_language: "Go"
category:             "Go"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

새 프로젝트를 시작하는 것은 비어있는 캔버스에서 그림을 그리는 것과 같습니다. 프로그래머는 새로운 기능을 제작하거나 문제를 해결하기 위해 새 프로젝트를 시작합니다.

## 방법:

새 Go 프로젝트를 시작하려면, 간단한 프로그램을 작성해봅니다. 예를 들면, 

```Go
package main

import "fmt"

func main() {
    fmt.Println("Hello, world!")
}
```

이 프로그램을 실행하면 아래와 같습니다:

```Go
Hello, world!
```

## 깊게 알아보기:

*역사적 맥락* - Go 언어는 2007년 구글에서 개발되어 널리 사용되게 되었습니다. 간결함, 빠른 컴파일 속도, 병렬 프로그래밍을 위한 강력한 지원 등이 주요 특징입니다.

*대안들* - Python, Java, Ruby 등 다양한 언어가 있지만 Go는 그 중에서도 빠른 개발 시간과 효율성이 뛰어나다는 점에서 선호됩니다.

*프로젝트에 대한 세부 사항* - Go 프로젝트는 일반적으로 하나의 메인 패키지와 여러 개의 보조 패키지로 구성됩니다. 프로젝트는 `go mod init` 명령어로 시작합니다.

## 참고 자료:

- Go 공식 웹사이트: https://golang.org/
- Go 프로젝트 관리에 대한 자세한 정보: https://golang.org/doc/tutorial/getting-started
- Go 언어에 대한 깊은 부분을 알고 싶다면: https://golang.org/doc/effective_go
- Go 언어에 대한 처음 시작하는 사람들을 위한 가이드: https://tour.golang.org/welcome/1