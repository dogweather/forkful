---
title:                "디렉토리가 존재하는지 확인하기"
html_title:           "Bash: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 무엇이며 왜?

디렉토리가 존재하는지 확인하는 것은 디렉토리의 상태를 검사하는 프로그래밍 작업입니다. 이 작업이 필요한 이유는 오류를 방지하고, 프로그램의 실행 흐름을 조절하기 위해서입니다.

## Java로 이렇게:
디렉토리가 존재하는지 확인하기 위해 Go 언어에서는 os 패키지의 Stat() 함수와 os.IsNotExist() 함수를 이용할 수 있습니다.

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    if _, err := os.Stat("/path/to/directory"); os.IsNotExist(err) {
        fmt.Println("Directory does not exist")
    } else {
        fmt.Println("Directory exists")
    }
}
```

## Deep Dive

디렉토리 존재 확인의 사용은 프로그래밍의 역사만큼 오래되었습니다. 과거에는 파일 시스템의 가용 공간 감시, 불필요한 덮어쓰기 방지 등으로 사용되었습니다. 최신 프로그래밍에서는 웹서버에 업로드된 파일의 저장 위치 확인, 시스템 설정 디렉토리 유무 검사 등 다양한 목적으로 쓰입니다.

현재 Go에서는 os 패키지의 Stat() 함수를 사용하여 파일이나 디렉토리의 정보를 가져오며, 그 후 os.IsNotExist() 함수를 통해 에러 타입이 ErrNotExist인지 확인함으로써 디렉토리의 존재를 검사합니다.

## See Also

- os 패키지 공식 문서: https://golang.org/pkg/os/
- 'os' 패키지 튜토리얼: https://golangdocs.com/os-package-in-go
- Go 언어 입문 가이드: https://www.learn-go-lang.org/basics/introduction