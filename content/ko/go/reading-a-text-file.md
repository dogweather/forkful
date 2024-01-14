---
title:    "Go: 텍스트 파일 읽기"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 읽는 것이 왜 중요한지 궁금하신가요? 텍스트 파일은 우리가 일상적으로 접하는 다양한 정보를 담고 있습니다. 예를 들어, 가장 간단한 형태의 텍스트 파일은 메모장에서 작성한 일기가 있을 수 있고, 더 복잡한 형태의 텍스트 파일은 소프트웨어 라이센스 파일 등이 있을 수 있습니다. 이러한 텍스트 파일을 읽는 것은 우리가 다양한 정보를 이해하고 활용하는 데 도움이 됩니다.

## 어떻게

텍스트 파일을 Go 언어로 읽는 방법은 다음과 같습니다.

```
package main

import (
    "fmt"
    "os"
    "bufio"
)

func main() {
    // 읽을 텍스트 파일 열기
    file, err := os.Open("sample.txt")
    if err != nil {
        fmt.Println(err)
    }
    defer file.Close()

    // 파일 안의 데이터를 한 줄씩 읽어오기
    scanner := bufio.NewScanner(file)
    for scanner.Scan() {
        fmt.Println(scanner.Text())
    }

    if err := scanner.Err(); err != nil {
        fmt.Println(err)
    }
}
```

위의 코드를 실행하면 `sample.txt` 파일 안의 모든 내용을 한 줄씩 읽어와서 출력해줍니다. 이를 통해 우리는 텍스트 파일 안의 데이터를 쉽게 읽고 활용할 수 있습니다.

## 딥 다이브

텍스트 파일을 읽는 과정에서 더 심화된 내용을 알고 싶으신가요? 이를 위해 우리는 파일 입출력 기능과 버퍼링에 대해 좀 더 자세히 알아보도록 하겠습니다.

파일 입출력 기능은 `os` 패키지 안에 있는 `Open()` 메소드와 `Close()` 메소드를 통해 구현할 수 있습니다. `Open()` 메소드는 파일을 읽기 위해 열고, `Close()` 메소드는 파일을 닫는 역할을 합니다. 또한 `bufio` 패키지의 `NewScanner()` 메소드를 사용하면 파일 안의 데이터를 한 줄씩 읽어올 수 있습니다. 이는 파일을 한 번에 전체를 읽는 것보다 효율적입니다.

버퍼링은 우리가 일상생활에서 자주 사용하는 말일 수 있습니다. 버퍼링은 데이터를 일시적으로 저장해두는 메모리 공간을 의미합니다. 파일을 읽을 때도 버퍼링을 사용하여 한 번에 많은 양의 데이터를 읽어오는 것이 효율적입니다. 이를 위해 `bufio` 패키지의 `NewScanner()` 메소드는 내부적으로 버퍼링 기능을 제공합니다.

## 더 알아보기

더 많은 정보를 원하신다면 아래 링크들을 참고해보세요.

- [Go 언어 텍스트 파일 입출력 구현 예제](https://zetawiki.com/wiki/Go_텍스트_파일_입출력)
- [Go 언어 버퍼링 기능 소개](https://go-tour-kr.appspot.com/#65)
- [Go 언어 공식 문서 - 파일](https://golang.org/pkg/os/#File)

## 봐도 좋아

- [Go 언어 공식 문서 - 버퍼링](https://golang.org/pkg/bufio/#Scanner)
- [Go 언어를 통한 파일 입출력의 기본 개념](https://www.inflearn.com/course/golang-%ED%8C%8C%EC%9D%B4%EB