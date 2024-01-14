---
title:    "Go: 문자열 연결하기"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/go/concatenating-strings.md"
---

{{< edit_this_page >}}

# 왜 Go에서 문자열을 연결하는 것인가?

문자열을 연결하는 것은 많은 개발자들에게 익숙한 일일 것입니다. 이것은 대부분의 프로그래밍 언어에서 가능한 기능이기 때문입니다. 그러나 그것이 Go에서도 가능하다는 것을 알고있다면, 더 빠르고 효율적인 코드를 작성할 수 있을 것입니다.

# Go에서 문자열을 연결하는 방법

Go에서 문자열을 연결하는 방법은 간단합니다. ```+``` 연산자를 사용하여 두 개의 문자열을 연결할 수 있습니다. 예를 들어:

```Go
package main

import "fmt"

func main() {
    firstString := "Hello"
    secondString := "world"
    fmt.Println(firstString + " " + secondString)
}
```

이 코드는 "Hello world"를 출력합니다. 또한 여러 개의 문자열을 연결할 수도 있습니다. 예를 들어:

```Go
package main

import "fmt"

func main() {
    firstString := "I"
    secondString := "love"
    thirdString := "Go"
    fmt.Println(firstString + " " + secondString + " " + thirdString)
}
```

이 코드는 "I love Go"를 출력합니다.

# 문자열 연결에 대해 더 알아보기

때로는 문자열을 연결하기 위해 다른 방법을 사용해야할 수 있습니다. ```fmt.Sprintf()``` 함수는 여러 문자열을 한 번에 연결하는 데 사용될 수 있습니다. 예를 들어:

```Go
package main

import "fmt"

func main() {
    firstString := "Hello"
    secondString := "world"
    fullString := fmt.Sprintf("%s %s!", firstString, secondString)
    fmt.Println(fullString)
}
```

이 코드는 "Hello world!"를 출력합니다.

# 자세히 알아보기

Go에서는 문자열 결합이 순서대로 수행되므로 많은 문자열을 결합할 때 ```bytes.Buffer```를 사용하는 것이 더 효율적일 수 있습니다. 또한 문자열 결합은 메모리 할당을 필요로하므로 큰 문자열의 경우 성능이 저하 될 수 있습니다.

# 더 자세한 정보를 원한다면...

교차 컴파일러와 제로 가비지 컬렉션과 같은 여러 가지 흥미로운 기능을 갖춘 Go에 대한 더 자세한 정보를 원한다면 아래의 링크를 확인해보세요.

## 참고자료

- Go 공식 웹사이트: https://golang.org/
- Go 언어 투어: https://tour.golang.org/
- Effective Go: https://golang.org/doc/effective_go.html
- Go 표준 라이브러리: https://golang.org/pkg/