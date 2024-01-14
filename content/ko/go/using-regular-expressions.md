---
title:    "Go: 정규식을 이용하는 방법"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜

정규 표현식을 사용해야 할까요? 정규 표현식은 문자열에서 패턴을 찾고 대체하거나 추출하는 데 유용합니다. 자주 사용되는 텍스트 처리 작업을 빠르고 쉽게 수행할 수 있습니다. 

## 사용 방법

정규 표현식을 사용하기 위해서는 `regexp` 패키지를 `import`해야 합니다. 그리고 `regexp.Compile()` 함수를 사용하여 정규식 패턴을 컴파일합니다. 컴파일된 패턴을 `regexp.MatchString()` 함수에 적용하여 문자열이 패턴과 일치하는지 검사할 수 있습니다.

```Go
package main

import (
    "fmt"
    "regexp"
)

func main() {
    pattern := regexp.Compile("Go[a-z]+") // "Go"로 시작하고 소문자가 1개 이상 있는 패턴을 컴파일
    fmt.Println(pattern.MatchString("GoLang")) // true 출력
    fmt.Println(pattern.MatchString("Java")) // false 출력
}
```

## 깊이 파고들기

정규 표현식을 더욱 자세히 살펴보겠습니다. 정규식 패턴은 문자열에서 패턴을 찾고 추출하는 데 사용됩니다. 이를 위해 다양한 메타 문자와 특수 문자를 알아야 합니다. 예를 들어, `.`은 어떤 문자가 와도 상관없다는 것을 의미하고 `+`는 앞에 오는 문자가 하나 이상일 때 일치한다는 것을 의미합니다. 이외에도 `\d`는 숫자, `\s`는 공백 문자를 나타내는 등 다양한 메타 문자와 연산자를 알아볼 수 있습니다. 정규식을 더욱 자세히 배우고 싶다면 아래의 링크를 참고하세요.

## 또 다른 참고 자료

- [Go 공식 문서 - 정규 표현식 패키지](https://golang.org/pkg/regexp/)
- [정규 표현식 테스트 및 실습 사이트 (정규식 101)](https://regex101.com/)
- [Go 정규 표현식을 편리하게 작성할 수 있는 패키지 (re2dfa)](https://github.com/opennota/re2dfa)