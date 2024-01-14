---
title:    "Go: 임시 파일 만들기"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜

컴퓨터 프로그래밍을 공부하거나 취미로 즐기는 사람이라면, 금방 알 수 있을 것입니다. 임시 파일은 우리에게 많은 편의를 제공합니다. 기본적으로, 우리는 이러한 파일을 사용하여 우리가 작업하는 동안 일시적으로 데이터를 저장할 수 있습니다.

## 방법

```Go
package main

import (
	"fmt"
	"io/ioutil"
	"os"
)

func main() {
	file, err := ioutil.TempFile("", "example") //임시 파일 생성
	if err != nil {
		fmt.Println(err)
	}
	defer os.Remove(file.Name()) //프로그램이 종료될 때 임시 파일 삭제

	//임시 파일에 데이터 쓰기
	fmt.Fprintf(file, "Hello, World!")

	//임시 파일 읽기
	data, err := ioutil.ReadFile(file.Name())
	if err != nil {
		fmt.Println(err)
	}

	fmt.Println(string(data)) //출력: Hello, World!
}
```

위의 예시 코드에서, 우리는 `ioutil` 패키지의 `TempFile` 함수를 사용하여 빈 임시 파일을 생성합니다. 이 함수는 첫 번째 매개변수로 경로를 받으며, 기본적으로 빈 문자열(`""`)을 전달하면 운영체제가 해당 파일을 생성할 경로를 지정하게 됩니다. 두 번째 매개변수로는 단순히 임시 파일의 이름을 지정합니다.

`ioutil` 패키지의 `ReadFile` 함수를 사용하여 파일을 읽을 수 있습니다. 이 함수는 파일의 경로를 매개변수로 받고, 읽은 데이터를 바이트 슬라이스 형태로 반환합니다.

## 심화 학습

임시 파일을 생성하는 방법에는 여러 가지가 있지만, 주로 `ioutil` 패키지의 `TempFile` 함수를 사용합니다. 이 함수는 운영체제에 따라 다르게 동작할 수 있으므로, 개발자는 이에 대해 검색하여 자세한 내용을 파악해야 합니다. 또한 임시 파일의 경로와 생성된 파일의 이름에는 규칙이 있으므로, 이에 대해서도 학습할 필요가 있습니다.

## 더 보기

- [Go - ioutil 패키지 문서](https://golang.org/pkg/io/ioutil/)
- [Go - os 패키지 문서](https://golang.org/pkg/os/)
- [Creating Temporary Files in Go](https://gobyexample.com/temporary-files)