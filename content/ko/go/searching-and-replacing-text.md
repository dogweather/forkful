---
title:    "Go: 텍스트 검색 및 대체하기"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜
텍스트를 검색하고 바꾸는 것에 참여하는 이유는 간단합니다. 이 작업은 반복적이고 잦은 수정을 수행하는 데 매우 유용합니다.

## 방법
문자열을 바꾸는 작업은 Go에서 매우 간단합니다. 다음 예제 코드를 살펴보세요.

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "안녕하세요, 여러분!"
	fmt.Println(str)
	fmt.Println(strings.Replace(str, "여러분", "친구들", -1))
}
```
출력:

```
안녕하세요, 여러분!
안녕하세요, 친구들!
```

위 코드에서는 `strings.Replace` 함수를 사용하여 "여러분"을 "친구들"로 바꿨습니다.
마지막 매개변수인 `-1`은 모든 출현을 바꾸는 것을 의미합니다. 따라서 `str` 변수의 모든 "여러분"이 "친구들"로 바뀌었습니다.

## 딥 다이브
위 코드에서는 `strings.Replace` 함수를 사용했지만, Go에는 다양한 다른 문자열 검색 및 교체 함수가 있습니다. `strings.Contains`, `strings.Count`, `strings.Index` 등으로 문자열을 비교하거나 위치를 찾을 수도 있습니다. 또한 정규표현식을 사용하여 더 복잡한 검색과 교체를 할 수도 있습니다.

## 참고 자료
- [Go 문자열 함수](https://golang.org/pkg/strings/)
- [Go 정규 표현식](https://golang.org/pkg/regexp/)
- [예제와 함께 배우는 Go](https://pyrasis.com/book/GoForTheReallyImpatient/)