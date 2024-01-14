---
title:    "Go: 문자열의 길이 찾기"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# 왜
문자열의 길이를 검색하는 일을 하는 이유는 문자열을 다루는 프로그래밍 작업에서 매우 중요하고 기본적인 동작이기 때문입니다. 예를 들어, 사용자로부터 입력받은 데이터를 검증하거나, 문자열을 잘라내는 등의 작업을 할 때 문자열의 길이를 알아야 합니다.

## 어떻게
문자열의 길이를 확인하는 방법은 간단합니다. 우선 Go 프로그래밍 언어에서 제공하는 내장 함수인 `len()`을 사용합니다. 이 함수는 어떤 타입의 값이든 해당하는 길이를 반환합니다. 다음은 간단한 예제 코드와 샘플 출력입니다.

```Go
package main

import "fmt"

func main() {
    str := "Hello, World!"
    fmt.Println(len(str))
}
```

출력:
```
13
```

위의 예제에서는 `len()` 함수를 사용해 문자열 `Hello, World!`의 길이를 구하고 출력해 보았습니다. 문자열의 길이는 공백을 포함한 모든 문자의 개수를 뜻하므로 13이 출력됩니다. 이와 같이 간단하게 `len()` 함수를 사용하여 문자열의 길이를 구할 수 있습니다.

## 딥 다이브
`len()` 함수는 사실 내부적으로 문자열을 바이트 단위로 분석해 길이를 계산합니다. 문자 하나마다 1바이트씩의 공간을 차지하며, 한글과 같은 멀티바이트 문자는 여러 바이트를 차지하게 됩니다. 따라서 이를 고려하여 문자열의 길이를 구하려면 바이트 길이를 단순하게 세는 것이 아니라 더 복잡한 과정이 필요하게 됩니다. 더욱 자세한 내용은 관련 문서를 참고하시기 바랍니다.

# 참고
- [Go 개발자 공식 문서](https://golang.org/doc/)
- [Go 언어 코딩 예제](https://gobyexample.com/hello-world)
- [문자열 조작에 관한 블로그 포스트](https://blog.golang.org/strings)