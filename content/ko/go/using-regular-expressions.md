---
title:    "Go: 정규식 사용하기"
keywords: ["Go"]
---

{{< edit_this_page >}}

## 왜?

정규 표현식을 사용하는 이유는 인간의 언어가 각각 다른 다양한 패턴을 가지고 있기 때문입니다. 따라서 프로그래밍 언어에서도 이러한 패턴을 처리하기 위해 정규 표현식이 필요합니다. Go 언어에서는 간단한 문법으로 정규 표현식을 다룰 수 있기 때문에, 누구나 쉽게 알아서 사용할 수 있습니다.

## 사용 방법

Golang에서 정규 표현식을 사용하는 방법은 매우 간단합니다. 먼저 `regexp` 패키지를 import 해야 합니다. 그리고 `regexp.Compile()` 함수를 사용해서 원하는 패턴을 컴파일하고, `MatchString()` 함수를 사용해서 원하는 문자열과 매치되는지 확인할 수 있습니다.

```
package main

import (
    "fmt"
    "regexp"
)

func main() {
    reg := regexp.MustCompile(`[가-힣]+`)
    fmt.Println(reg.MatchString("안녕하세요"))  // true
    fmt.Println(reg.MatchString("hello"))  // false
}
```

## 깊이 파고들기

정규 표현식은 일반적으로 사용되는 단순한 패턴 매칭뿐만 아니라 고급 기능도 제공합니다. 예를 들어, `FindString()` 함수를 사용하면 정규 표현식에 해당하는 첫 번째 문자열을 찾을 수 있고, `FindAllString()` 함수를 사용하면 정규 표현식에 해당하는 모든 문자열을 찾을 수 있습니다.

또한, 정규 표현식에서 그룹을 지정할 수도 있습니다. 이는 특정 부분 문자열만 추출하거나, 그룹에 해당하는 부분을 다른 문자열로 바꿀 때 유용합니다.

## 참고 자료

- [Golang 정규 표현식 문서](https://golang.org/pkg/regexp/)
- [Go 언어 정규 표현식 테스트기](https://regex-golang.appspot.com/)
- [Efficient and Complete Regular Expression Matching in Go](https://www.youtube.com/watch?v=Z_eALOCm8QI)