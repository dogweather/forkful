---
title:                "텍스트 파일 읽기"
html_title:           "Go: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜
텍스트 파일을 읽는 것을 알고리즘의 기본 중 하나이기 때문에, 새로운 프로그래머로서 중요한 기술을 배우기 위해 이 글을 읽을 수 있습니다.

## 하우 투
텍스트 파일을 읽는 방법은 매우 간단합니다. 우선, "io" 패키지를 임포트해주세요. 그리고 아래의 코드를 참고하여 파일을 열고 읽어보세요.

```Go
package main

import (
    "fmt"
    "io/ioutil"
)

func main() {
    // 파일을 읽고 싶은 경로를 지정합니다.
    path := "textfile.txt"

    // 파일을 열고 오류가 있는지 확인합니다.
    file, err := ioutil.ReadFile(path)
    if err != nil {
        fmt.Println("파일을 열 수 없습니다.")
        return
    }

    // 파일의 내용을 출력합니다.
    fmt.Println("파일 내용:", string(file))
    // 파일을 닫습니다.
    defer file.Close()
}
```

위의 예시 코드를 실행하면, "textfile.txt"에 있는 내용을 읽고 콘솔에 출력할 수 있습니다. 이제 여러분은 텍스트 파일을 읽는 방법을 배웠습니다!

## 딥 다이브
파일 읽기는 "io/ioutil" 패키지를 사용하는 것 외에도 "bufio" 패키지를 사용할 수도 있습니다. 이 패키지를 사용하면 읽은 내용을 처리하기 더 쉬워질 수 있습니다. 또한, "os" 패키지를 사용하여 파일을 열고 닫는 것도 가능합니다. 다른 방법들을 찾아보고 익숙해지면 여러분만의 파일 읽기 방법을 개발해보세요!

## See Also
- [Go 공식 문서](https://golang.org/doc/)
- [Go 언어 투어](https://go-tour-kr.appspot.com/)
- [파일 읽고 쓰기](https://mingrammer.com/gobyexample/reading-files/)