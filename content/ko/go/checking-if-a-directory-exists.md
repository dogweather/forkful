---
title:                "디렉토리가 존재하는지 확인하기"
html_title:           "Go: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 뭐고 왜? 
디렉토리가 존재하는지 확인하는 것은 프로그래머들이 파일이나 디렉토리를 조작하기 전에 그 존재를 확인하기 위해 하는 것입니다.

## 어떻게: 
```Go
// 디렉토리가 존재하는지 확인하는 예제 코드
package main

import (
	"fmt"
	"os"
)

func main() {
	dirName := "/home/test"
	_, err := os.Stat(dirName)

	if os.IsNotExist(err) {
		fmt.Println("디렉토리가 존재하지 않습니다.")
	} else {
		fmt.Println("디렉토리가 존재합니다.")
	}
}

// 예제 출력
// 디렉토리가 존재하지 않습니다.
```


## Deep Dive: 
디렉토리의 존재를 확인하는 기능은 파일 시스템에 대한 접근을 위해 필수적입니다. 이 기능은 파일 시스템에서 파일을 생성하거나 수정하기 전에 디렉토리가 있는지 먼저 확인하는 용도로 많이 사용됩니다. 옛날에는 C언어의 stat 함수를 사용하여 디렉토리의 존재를 확인했지만, Go 언어에서는 os.Stat 함수를 사용하면 쉽게 구현할 수 있습니다.

## See Also:
- [Go 언어 문서](https://golang.org/pkg/os/#Stat)
- [디렉토리 접근 권한 관련 블로그 포스트](https://blog.golang.org/pipelines)