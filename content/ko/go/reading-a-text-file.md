---
title:                "텍스트 파일 읽기"
html_title:           "Bash: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

텍스트 파일 읽기는 프로그램이 일련의 문자들을 파일로부터 불러 보는 것을 의미합니다. 이 작업을 통해 프로그래머들은 데이터를 처리하고, 분석할 수 있고, 다른 곳에서 생성된 정보를 쉽게 가져올 수 있습니다.

## 사례:

텍스트 파일을 읽기 위해 Go에서는 `os`와 `bufio` 패키지를 사용하는 것이 일반적입니다. 아래는 간단한 예시입니다.

```Go
package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
    file, err := os.Open("test.txt")
    if err != nil {
		log.Fatal(err)
    }
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		fmt.Println(scanner.Text())
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}
}
```

이 코드는 "test.txt" 라는 파일을 읽어, 각 라인을 콘솔에 출력합니다. 오류 검사를 통해 파일 열기나 읽기 과정에 발생할 수 있는 문제를 처리합니다.

## 깊이 있게 알아보기:

텍스트 파일 읽기는 소프트웨어의 초창기 부터 일반적으로 사용되는 기능입니다. Go의 경우 `os`와 `bufio` 패키지를 사용하는 것이 일반적이지만 `ioutil` 패키지를 사용할 수도 있습니다. 하지만, Go 1.16 이후로 `ioutil`은 deprecated되어 권장되지 않습니다. 이 패키지 대신 `os`와 `io`, `bufio` 패키지를 사용하는 것이 더 좋은 배려를 미치게 될 것입니다.

## 참조:

1. 공식 Go 텍스트 파일 처리 문서: https://pkg.go.dev/bufio
2. Go 언어를 사용한 파일 읽기와 쓰기에 대한 더 많은 정보: https://golangbot.com/read-files/ 
3. `ioutil` 패키지 Deprecated 정보: https://golang.org/doc/go1.16#ioutil