---
title:                "Go: 정규 표현식 사용하기"
simple_title:         "정규 표현식 사용하기"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜
정규표현식을 사용하는 이유입니다. 
정규표현식은 문자열에서 패턴을 찾고 조작하는 유용한 도구입니다. 따라서 문자열과 관련된 작업을 할 때 매우 유용합니다.

## 사용 방법
아래의 예제 코드를 통해 정규표현식을 사용하는 방법을 살펴보겠습니다. 

```Go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	// 패턴을 컴파일합니다.
	re := regexp.MustCompile(`\d+`)

	// 매칭할 문자열입니다.
	str := "I have 10 apples and 5 bananas."

	// 정규표현식을 사용하여 매칭되는 모든 부분을 출력합니다.
	fmt.Printf("%q\n", re.FindAllString(str, -1))
}
```
**출력 결과:**
`["10", "5"]`

위의 예제 코드에서는 `\d+`라는 패턴을 사용하여 매칭되는 숫자를 찾아서 출력합니다. 정규표현식 내에서 사용되는 특수문자들을 잘 활용하면 다양한 패턴을 찾아낼 수 있습니다.

## 깊이있게 살펴보기
정규표현식을 사용할 때 유의해야 할 몇 가지 중요한 점이 있습니다.

- 패턴을 컴파일할 때 `regexp.MustCompile()` 함수를 사용하여 컴파일하는 것이 좋습니다. 이를 통해 패턴이 유효한지 컴파일 전에 확인하는 과정을 거치게 됩니다.
- `FindAllString()` 함수를 사용하여 정규표현식에 매칭되는 모든 부분을 찾아출력할 수 있습니다.
- 정규표현식 내에서 사용되는 특수문자들을 활용하여 다양한 패턴을 만들어낼 수 있습니다. 따라서 꼭 필요한 경우가 아니라면 정규표현식 외의 방법으로 문자열을 다루는 것이 더 효율적입니다.

## 관련 링크
- Go 언어 공식 문서: https://golang.org/pkg/regexp/
- 정규표현식 빌더 사이트: https://regexr.com/
- Go 언어로 정규표현식 사용하기: https://www.digitalocean.com/community/tutorials/how-to-use-regular-expressions-in-go-ko