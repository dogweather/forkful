---
title:                "Go: 디렉토리가 존재하는지 확인하는 방법"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# 왜 디렉토리가 존재하는지 확인해야 할까요?

디렉토리가 존재하는지 확인하는 작업은 프로그래밍에서 매우 중요합니다. 파일을 생성하거나 읽을 때, 디렉토리가 존재하지 않는다면 이를 처리하기 위한 방법이 필요합니다. 또는 이미 존재하는 디렉토리에 대해 중복 생성을 막기 위해서도 디렉토리가 존재하는지 확인하는 것이 필요합니다. 이 글에서는 Go 언어로 디렉토리가 존재하는지 확인하는 방법을 알아보겠습니다.

## 어떻게 디렉토리 존재 여부를 확인할 수 있나요?

Go 언어에서는 `os.Stat()` 함수를 사용하여 디렉토리 존재 여부를 확인할 수 있습니다. 아래는 `directory` 변수에 있는 디렉토리가 존재하는지 확인하는 예시 코드입니다.

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    directory := "example/directory"

    if _, err := os.Stat(directory); os.IsNotExist(err) {
        fmt.Println("디렉토리가 존재하지 않습니다.")
    } else {
        fmt.Println("디렉토리가 존재합니다.")
    }
}
```

위 코드에서 `os.Stat()` 함수는 첫 번째 파라미터로 받은 디렉토리 경로의 파일 정보를 리턴합니다. 만약 디렉토리가 존재하지 않는다면 `os.IsNotExist()` 함수를 사용하여 이를 확인할 수 있습니다. 존재하지 않는 디렉토리 경로에 대해서만 `os.Stat()` 함수는 에러를 리턴하기 때문입니다.

## 디렉토리 존재 여부에 대해 더 깊게 알아보기

Go 언어에서는 `os.Stat()` 함수만을 사용하여 디렉토리 존재 여부를 확인할 수 있지만, 좀 더 자세한 정보를 알기 위해서는 `os.FileInfo` 타입의 메소드를 사용할 수 있습니다. 이 메소드들을 사용하면 파일의 이름, 수정일자, 크기 등 다양한 정보를 얻을 수 있습니다.

아래는 `Stat()` 함수를 사용해 디렉토리의 파일 정보를 얻은 다음, `os.FileInfo` 메소드를 사용하여 해당 정보를 출력하는 예시 코드입니다.

```
func main() {
    directory := "example/directory"

    fileInfo, err := os.Stat(directory)
    if err != nil {
        fmt.Println("에러 발생:", err)
    }

    fmt.Println("파일 이름:", fileInfo.Name())
    fmt.Println("수정일자:", fileInfo.ModTime())
    fmt.Println("파일 크기:", fileInfo.Size())
}
```

이 외에도 `os.FileInfo` 타입에는 디렉토리에 속한 파일들의 리스트를 얻을 수 있는 `Readdir()` 메소드나 디렉토리의 권한을 확인하는 `Mode()` 메소드 등이 있습니다.

## 다른 정보를 확인해보세요

이 글에서는 Go 언어로 디렉토리 존재 여부를 확인하는 방법을 간략하게 알아보았습니다. 디렉토리 처리와 관련된 더 많은 정보는 공식 Go 언어 문서나 다른 블로그 글에서 확인할 수 있습니다.

## 더 많은 정보

- [Go 공식 문서: os 패키지](https://golang.org/pkg/os/)
- [Go 공식 문서: os.FileInfo 인터페이스](https://golang.org/pkg/os/#FileInfo)
- [Golangbot: 디렉토리 다