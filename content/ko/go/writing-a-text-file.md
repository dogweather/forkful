---
title:                "텍스트 파일 작성하기"
date:                  2024-01-19
simple_title:         "텍스트 파일 작성하기"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)

텍스트 파일 쓰기란 문자열 정보를 파일로 저장하는 것입니다. 프로그래머들은 데이터를 영속적으로 보관하고, 나중에 재사용하기 위해 이 작업을 합니다.

## How to: (방법)

```Go
package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	file, err := os.Create("example.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	writer := bufio.NewWriter(file)
	_, err = writer.WriteString("안녕하세요, Go 언어 파일 쓰기 예시입니다!")
	if err != nil {
		panic(err)
	}
	writer.Flush()
}
```

출력 없음(파일에 "안녕하세요, Go 언어 파일 쓰기 예시입니다!" 문자열이 저장됨).

## Deep Dive (자세히 알아보기)

Go 언어에서 텍스트 파일 쓰기는 초기 개발부터 중요한 기능이었습니다. `os`와 `bufio`와 같은 표준 라이브러리는 효율적인 파일 IO를 지원합니다. 대안으로는 `io/ioutil` 패키지의 `WriteFile` 함수를 사용할 수도 있으나 Go 1.16 이후에는 `os.WriteFile`을 권장합니다. `bufio.Writer`를 사용하면 버퍼링을 통해 성능을 최적화할 수 있습니다.

## See Also (관련 자료)

- Go by Example: Writing Files: [https://gobyexample.com/writing-files](https://gobyexample.com/writing-files)
- Go documentation for the os package: [https://pkg.go.dev/os](https://pkg.go.dev/os)
- Go documentation for the bufio package: [https://pkg.go.dev/bufio](https://pkg.go.dev/bufio)
