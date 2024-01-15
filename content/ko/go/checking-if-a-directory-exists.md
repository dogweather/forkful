---
title:                "디렉토리의 존재 여부 확인"
html_title:           "Go: 디렉토리의 존재 여부 확인"
simple_title:         "디렉토리의 존재 여부 확인"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜?

이 문제는 사람들이 특정 디렉토리를 찾는데 어려움을 겪는 경우에 직면할 수 있기 때문에 중요합니다.

## 어떻게?

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    path := "./my_directory" // 존재하는 디렉토리인지 확인할 경로 설정
    if _, err := os.Stat(path); os.IsNotExist(err) { // 해당 경로의 파일이나 디렉토리가 존재하는지 확인
        fmt.Println("해당 디렉토리는 존재하지 않습니다.")
    } else {
        fmt.Println("해당 디렉토리가 존재합니다.")
    }
}
```

위의 예시 코드에서는 `os.Stat()` 함수를 사용하여 경로의 파일이나 디렉토리가 존재하는지 확인합니다. 이 함수는 두 가지 값을 반환하는데, 하나는 `os.FileInfo` 타입의 값이고 다른 하나는 `error` 타입의 값입니다. 이 함수의 반환값을 이용하여 해당 경로의 파일이나 디렉토리가 있는지 확인할 수 있습니다. `os.IsNotExist(err)` 함수를 사용하여 해당 경로의 파일이나 디렉토리가 존재하지 않는 경우를 처리할 수 있습니다.

### 출력 예시

```
해당 디렉토리가 존재하지 않습니다.
```

```
해당 디렉토리는 존재합니다.
```

## 깊이 파고들기

`os.Stat()` 함수는 해당 경로의 파일이나 디렉토리가 존재하는지 뿐만 아니라 다른 정보들도 반환합니다. 예를 들어, 해당 파일이나 디렉토리의 권한, 변경 시간 등 다양한 정보를 얻을 수 있습니다. 이러한 정보는 파일 이외의 디렉토리에 대해서도 확인할 수 있습니다.

## 참고 자료

- [Go documentation - Package os](https://golang.org/pkg/os/)
- [Go documentation - os.Stat](https://golang.org/pkg/os/#Stat)
- [The Go Programming Language Specification - Comparison operators](https://golang.org/ref/spec#Comparison_operators)
- [Go by Example - Check if a file exists](https://gobyexample.com/file-exists)