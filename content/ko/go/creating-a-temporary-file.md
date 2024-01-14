---
title:                "Go: 임시 파일 만들기"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜

임시 파일을 만들고 사용하는 이유는 다양합니다. 가장 일반적인 이유는 프로그램이 런타임 동안 데이터를 저장하고 재사용하기 위해서입니다. 또 다른 이유는 프로그램이 동작 중에 임시 파일을 사용하여 다른 프로그램과의 데이터 교환을 할 수도 있습니다.

## 이 방법으로

임시 파일을 만드는 방법은 매우 간단합니다. 우선, `io/ioutil` 패키지를 임포트합니다. 그리고 `ioutil.TempFile()` 함수를 사용하여 임시 파일 객체를 생성합니다. 아래는 이 과정을 보여주는 코드 예시입니다.

```Go
import (
    "fmt"
    "io/ioutil"
)

func main() {
    // 임시 파일 생성
    tempFile, err := ioutil.TempFile("", "example")
    if err != nil {
        panic(err)
    }

    // 임시 파일이 생성된 경로와 파일명 출력
    fmt.Println("임시 파일 경로:", tempFile.Name())

    // 임시 파일 사용 후 삭제
    defer os.Remove(tempFile.Name())
}
```

위 코드는 `ioutil` 패키지에서 제공하는 `TempFile()` 함수를 이용하여 임시 파일을 생성하고 그 경로를 출력하는 예시입니다.

## 더 깊게 알아보기

`ioutil` 패키지가 제공하는 `TempFile()` 함수는 두 개의 매개변수를 가지는데, 첫 번째는 생성될 임시 파일이 저장될 디렉토리 경로이고, 두 번째는 임시 파일의 접두사로 사용될 문자열입니다. 두 번째 매개변수는 옵션이며, 입력하지 않을 경우 임시 파일 이름은 "tmp"로 시작됩니다.

## 또 다른 정보 확인

임시 파일을 사용하는 다른 방법에 대해 더 깊이 알아보고 싶다면, 아래 링크들을 참고해보세요.

- [Go 표준 라이브러리 패키지 ioutil](https://golang.org/pkg/io/ioutil/)
- [임시 파일을 사용하여 데이터 교환하기 예시](https://yourbasic.org/golang/temporary-file-directory/)

## 참고

위 코드 예시에서는 `defer` 구문을 사용하여 임시 파일을 사용한 뒤에 삭제하도록 하였습니다. 이는 임시 파일을 사용한 뒤에 명시적으로 삭제하지 않아도 자동으로 삭제되도록 해줍니다.