---
title:                "텍스트 파일 읽기"
date:                  2024-01-20T17:54:42.893113-07:00
model:                 gpt-4-1106-preview
simple_title:         "텍스트 파일 읽기"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
텍스트 파일 읽기는 파일의 내용을 불러오는 것입니다. 프로그래머들은 데이터 처리, 설정 불러오기, 로그 분석 등을 위해 파일을 읽습니다.

## How to: (방법)
Go에서 텍스트 파일을 읽는 기본적인 예제입니다:

```go
package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

func main() {
	file, err := os.Open("example.txt") // 파일 열기
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close() // 나중에 파일 닫기

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		fmt.Println(scanner.Text()) // 파일의 각 줄 출력
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}
}
```

이 코드는 `example.txt` 파일을 열고, 각 줄을 읽어서 출력합니다. 파일이 제대로 밀리거나 없는 경우는 로그에 에러를 기록합니다.

```go
Hello, Go!
텍스트 파일 예제입니다.
```

## Deep Dive (깊이 있는 정보)
오래 전부터 텍스트 파일 읽기는 프로그래밍의 기본 중 하나로 자리잡았습니다. Go에서는 `os`와 `bufio` 라이브러리를 통해 효율적인 파일 읽기를 제공합니다. `ioutil.ReadFile` 같은 간편 함수도 있지만, 대량의 데이터를 처리할 때는 `bufio.Scanner`를 사용하는 것이 좋습니다. 메모리를 더 효율적으로 사용하기 때문입니다. 

파일 읽기의 다양한 방법 중 `ioutil`, `os`, `bufio`가 주로 사용되며, Go 1.16 버전부터는 `io`와 `os` 패키지가 더 강화된 `io.ReadFile` 함수를 포함하고 있습니다.

## See Also (더 보기)
- Go by Example: Reading Files: https://gobyexample.com/reading-files
- The Go Blog: Defer, Panic, and Recover: https://blog.golang.org/defer-panic-and-recover
- Go Documentation for the os package: https://pkg.go.dev/os
