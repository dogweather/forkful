---
title:                "Go: 새 프로젝트 시작하기"
simple_title:         "새 프로젝트 시작하기"
programming_language: "Go"
category:             "Go"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 왜 Go 프로그래밍에 뛰어 들어야 할까요?

새로운 프로젝트를 시작하는 이유는 다양합니다. 몇 가지 예를 들자면, 새로운 기술을 배우고 싶다거나, 현재 사용 중인 언어나 프레임워크보다 더 효율적인 방법이 있을 수 있습니다. 어떤 이유로든 Go로 새로운 프로젝트를 시작하는 것은 매우 흥미로운 선택이 될 수 있습니다.

## 어떻게 시작할까요?

먼저 가장 간단한 "Hello World" 프로그램부터 시작해보겠습니다. 다음 코드를 작성해보세요.

```Go 
package main

import "fmt"

func main() {
	fmt.Println("Hello World!")
}
```

위 코드를 실행하면 다음과 같은 출력이 나옵니다.

```
Hello World!
```

이제 좀 더 복잡한 예제를 다루어 보겠습니다. 다음은 사용자로부터 이름을 입력받아 인사하는 프로그램입니다. 아래 코드를 보고 이해해보세요.

```Go 
package main

import (
	"fmt"
	"bufio"
	"os"
)

func main() {
	fmt.Println("이름을 입력하세요:")
	reader := bufio.NewReader(os.Stdin)
	name, _ := reader.ReadString('\n')
	fmt.Println("안녕하세요, " + name + "님!")
}
```

위 코드를 실행하면 사용자가 입력한 이름에 따라 다른 인사말이 출력됩니다.

```
이름을 입력하세요:
John
안녕하세요, John님!
```

## 딥 다이브

새로운 프로젝트를 시작할 때는 명확한 목표와 계획이 필요합니다. 먼저 어떤 문제를 해결하려는지 명확하게 정의하고, 그에 따른 목표를 설정하세요. 그 후에는 어떤 기술이나 프레임워크를 사용할지 결정하는 것이 좋습니다. Go는 빠르고 가독성이 좋고 안전한 언어로, 다양한 프로젝트에 적합합니다.

또한 늘 상상력을 발휘해보세요. 새로운 프로젝트에서는 어떤 기능을 넣어볼 수 있을까요? 어떤 방식으로 개선을 해볼 수 있을까요? 다양한 아이디어를 생각해보고, 그 중에서 가장 적합하고 흥미로운 것을 선택하세요.

## 더 알아보기

- [Go 공식 문서](https://golang.org/doc/)
- [효율적인 Go 코딩하기](https://medium.com/@kdnotes/efficiency-with-go-coding-conventions-1c16bf521810)
- [Go로 간단한 웹 서버 만들기](https://opensource.com/article/17/10/golang-for-python-developers)
- [Go 언어를 이용한 동시성 프로그래밍](https://speakerdeck.com/kennygrant/concurrency-in-go-avoiding-traps-and-pitfalls)