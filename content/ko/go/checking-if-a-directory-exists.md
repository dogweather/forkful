---
title:                "디렉토리 존재 여부 확인하기"
date:                  2024-01-20T14:56:54.135258-07:00
html_title:           "Fish Shell: 디렉토리 존재 여부 확인하기"
simple_title:         "디렉토리 존재 여부 확인하기"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가요? & 왜 하나요?)
디렉터리가 존재하는지 확인하는 것은 파일 시스템에서 특정 경로에 폴더가 있는지 검사합니다. 프로그래머들은 파일을 만들거나, 읽거나, 에러를 방지하기 위해서 이 작업을 합니다.

## How to (어떻게 하나요?)
Go에서 디렉터리 존재 여부를 확인하려면 `os.Stat()` 함수와 `os.IsNotExist()` 함수를 조합합니다.

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	// 확인하고 싶은 디렉터리 경로
	dirPath := "/path/to/directory"

	// 디렉터리 존재 여부를 확인
	if _, err := os.Stat(dirPath); os.IsNotExist(err) {
		fmt.Printf("Directory does not exist: %s\n", dirPath)
	} else {
		fmt.Printf("Directory exists: %s\n", dirPath)
	}
}
```

예상 출력:
- 디렉토리가 없을 때: `Directory does not exist: /path/to/directory`
- 디렉토리가 있을 때: `Directory exists: /path/to/directory`

## Deep Dive (심층 탐구)
역사적으로, 디렉터리 존재 여부는 파일 시스템의 기본 작업 중 하나입니다. Go 언어는 이를 위해 표준 라이브러리의 `os` 패키지를 제공합니다.

`os.Stat()`는 파일이나 디렉터리에 대한 정보를 받아오고, 실패할 경우 에러를 반환합니다. `os.IsNotExist()` 함수는 에러가 파일이나 디렉터리가 존재하지 않을 때 발생하는지를 판별합니다.

`os.Stat()` 대신 `os.IsExist()`를 사용할 수도 있지만, 주로 오류 처리에서 사용됩니다. 또, `ioutil.ReadDir()` 함수로 디렉터리에 있는 파일 목록을 검사하여 디렉터리 존재 여부를 간접적으로 알 수도 있습니다.

디렉터리 존재 여부를 확인하는 것은 파일 조작, 설치 스크립트, 버퍼 오버플로 방지 등 다양한 상황에서 중요합니다. 특히, Go 언어에서는 에러 처리가 핵심 철학 중 하나이므로, 이런 검사를 통해 프로그램의 안정성을 향상시킬 수 있습니다.

## See Also (관련 자료)
- Go 언어 공식 문서: https://golang.org/pkg/os/#Stat
- Go by Example - Directories: https://gobyexample.com/directories
- Go Blog — Error handling: https://blog.golang.org/error-handling-and-go

이 글을 통해 쉽게 디렉터리가 있는지 확인할 수 있게 되셨길 바랍니다. 필요한 정보를 더 탐색하려면 위의 링크들을 활용하세요!
