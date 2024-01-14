---
title:                "Go: 텍스트 검색 및 대체하기"
simple_title:         "텍스트 검색 및 대체하기"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# 왜
어떤 사람들이 텍스트를 찾고 바꾸는 작업을 수행할까요? 이 글에서는 텍스트를 찾고 바꾸는 작업의 중요성과 그에 따른 장점에 대해 간략하게 알아보겠습니다.

## 어떻게
텍스트를 찾고 바꾸는 작업은 Go 프로그래밍 언어 내에서 간단하게 수행할 수 있습니다. 아래의 예제 코드를 통해 실제 코딩 과정을 살펴보도록 하겠습니다.

```
package main

import (
    "fmt"
    "strings"
)

func main() {
    // 텍스트에서 "Hello"를 "안녕하세요"로 바꾸는 예제
    text := "Hello World!"
    newText := strings.Replace(text, "Hello", "안녕하세요", 1)
    
    fmt.Println(newText)
}
```

위와 같은 코드를 실행하면 "안녕하세요 World!"라는 결과가 출력됩니다. 이처럼 Replace 함수를 이용해서 텍스트 내에서 원하는 부분을 찾아서 바꾸는 작업을 수행할 수 있습니다.

## 깊이 파고들기
텍스트를 찾고 바꾸는 작업을 수행할 때, 정규표현식을 사용해서 원하는 패턴을 찾을 수도 있습니다. 아래의 코드는 정규표현식을 이용해 이메일 주소를 찾아서 바꾸는 예제입니다.

```
package main

import (
    "fmt"
    "regexp"
)

func main() {
    // 이메일 주소에서 도메인을 "example.com"으로 변환하는 예제
    email := "test@test.com"
    domainRegex := regexp.MustCompile("@([a-z]+)")
    newEmail := domainRegex.ReplaceAllString(email, "@example.com")

    fmt.Println(newEmail)
}
```

위의 코드를 실행하면 "test@example.com"이라는 결과가 출력됩니다. 정규표현식을 이용하면 조금 더 복잡한 패턴을 찾아서 원하는 대로 바꿀 수 있습니다.

# 참고 자료
- [Go strings.Replace function documentation](https://golang.org/pkg/strings/#Replace)
- [Go regexp package documentation](https://golang.org/pkg/regexp/#ReplaceAllString)