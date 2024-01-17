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

## 무엇이고 왜?

텍스트 파일을 읽는 것은 컴퓨터에서 저장된 데이터를 읽는 방식이다. 프로그래머들은 파일을 읽는 것을 통해 컴퓨터 내부의 데이터를 사용하여 자신이 원하는 결과를 얻을 수 있다.

## 어떻게:

``` Go
package main

import (
	"fmt"
	"io/ioutil"
)

func main() {
	// 파일의 내용 전체를 읽기
	content, err := ioutil.ReadFile("myfile.txt")
	if err != nil {
		fmt.Println("파일을 읽지 못했습니다.")
		return
	}
	fmt.Println(string(content))

	// 한 줄씩 읽기
	file, err := os.Open("myfile.txt")
	if err != nil {
		fmt.Println("파일을 열지 못했습니다.")
		return
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		fmt.Println(scanner.Text())
	}
}
```

**출력:**
```
저장된 데이터입니다.
한 줄씩 읽기 테스트입니다.
```

## 깊이 파고들기:

읽기 기능은 프로그래밍 언어나 컴퓨터 운영체제의 기본적인 기능 중 하나이다. 텍스트 파일을 읽는 것 외에도, 데이터베이스나 네트워크 소켓 등과 같은 다른 방식으로 저장된 데이터도 읽을 수 있다. 또한 읽기 기능을 통해 데이터를 가공하고 다른 포맷으로 저장할 수도 있다.

다른 언어에서도 파일을 읽는 방식은 유사하다. Go에서 파일을 읽는 방법은 매우 간단하며, 위 코드에서도 볼 수 있듯이 `ioutil.ReadFile`이나 `bufio.Scanner`를 통해 쉽게 파일을 읽을 수 있다.

파일을 읽는 과정에서 발생할 수 있는 에러에 대한 처리 또한 중요하다. 위 코드에서는 `err` 변수를 이용하여 에러 발생 시 처리하는 방법을 보여주었다.

## 관련 자료:

- [golang.org/pkg/io/ioutil](https://golang.org/pkg/io/ioutil)
- [golang.org/pkg/bufio](https://golang.org/pkg/bufio)