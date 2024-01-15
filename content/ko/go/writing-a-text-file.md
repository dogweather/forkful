---
title:                "텍스트 파일 쓰기"
html_title:           "Go: 텍스트 파일 쓰기"
simple_title:         "텍스트 파일 쓰기"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 작성하는 이유는 여러 가지가 있지만, 대표적으로는 데이터를 보관하거나 공유하기 위해서입니다. 예를 들어, 시스템 설정 파일이나 로그 파일 등이 텍스트 파일로 작성됩니다.

## 작성하는 방법

텍스트 파일을 작성하는 방법은 매우 간단합니다. 먼저, Go 언어에서 파일을 다루기 위해 `os` 패키지를 임포트해야 합니다. 그 다음, `os.Create()` 함수를 이용하여 새로운 파일을 생성하고 `os.Write()` 함수를 이용하여 텍스트를 입력할 수 있습니다. 아래는 간단한 예제 코드입니다.

```Go
package main

import "os"

func main() {
    file, err := os.Create("sample.txt")      // 새로운 파일 생성
    if err != nil {
        panic(err)                           // 에러 처리
    }
    defer file.Close()                       // 작업이 종료되면 파일 닫기

    text := "This is a sample text."          // 파일에 쓸 텍스트
    _, err = file.Write([]byte(text))        // 파일에 텍스트 입력
    if err != nil {
        panic(err)                           // 에러 처리
    }

    fmt.Println("Text file created.")
}
```

위 코드를 실행하면 현재 디렉토리에 `sample.txt` 파일이 생성되고, 내용으로 `This is a sample text.`가 입력될 것입니다.

## 깊게 들어가기

텍스트 파일을 작성하는 과정에서는 `os.Create()` 함수가 실제로 파일을 생성하는 것이 아니라 운영체제에 파일을 요청하는 것입니다. 따라서 파일에 대한 작업이 모두 끝나면 `file.Close()` 함수를 호출하여 파일을 닫아주어야 합니다. 또한, `os.Write()` 함수는 바이트 형태의 데이터를 입력하므로 `[]byte` 형태로 변환해주어야 합니다. 이러한 세부 사항들을 모두 고려하여 텍스트 파일을 작성하는 것이 중요합니다.

## 관련 자료

- [Go 공식 문서 - os 패키지](https://golang.org/pkg/os/)
- [OS 파일 다루기 - go.dev](https://go.dev/blog/files)
- [텍스트 파일 다루기 예제 코드 - GeeksforGeeks](https://www.geeksforgeeks.org/write-data-in-a-text-file-int-struct-string/?ref=rp)