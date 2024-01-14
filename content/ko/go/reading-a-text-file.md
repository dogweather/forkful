---
title:    "Go: 텍스트 파일 읽기"
keywords: ["Go"]
---

{{< edit_this_page >}}

# 왜

텍스트 파일을 읽는 것에 대해 궁금해하는 사람들을 위해 이 블로그 포스트를 작성하였습니다. Go 언어로 텍스트 파일을 읽는 방법에 대해 알아보겠습니다. 

## 어떻게

아래는 텍스트 파일을 읽어서 내용을 출력하는 간단한 예제 코드입니다. 

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	file, err := os.Open("sample.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	buffer := make([]byte, 1024)
	for {
		data, err := file.Read(buffer)
		if data == 0 || err != nil {
			break
		}
		fmt.Println(string(buffer[:data]))
	}
}
```

위의 코드를 실행하면, "sample.txt" 파일의 내용이 한 줄씩 출력됩니다. 예제 코드를 보면, 우선 파일을 열고(err가 nil인지 확인하여), 파일을 닫아야하는 defer문이 있음을 알 수 있습니다. 그리고 파일의 내용을 읽어서 버퍼에 담아 한 줄씩 출력하고 있습니다.

## 깊게 파고들기

텍스트 파일을 읽는 데 있어서 고려해야 할 몇 가지 사항이 있습니다. 첫째, 파일의 절대 경로를 제대로 입력하는 것이 중요합니다. 상대 경로를 사용할 경우, 원하는 파일을 찾지 못해 프로그램이 제대로 동작하지 않을 수 있습니다. 둘째, 파일에 쓰인 내용이 많을 경우, 버퍼의 크기가 충분한지 확인해야 합니다. 충분하지 않다면, 파일의 내용을 모두 다 읽지 못해 원하는 결과를 얻지 못할 수 있습니다. 

# 또 다른 정보들

텍스트 파일을 읽는 방법에 대해 더 자세한 정보를 원한다면, 다음 링크들을 참고하세요.

- [Go 문서: 파일 읽기](https://golang.org/pkg/os/#File)
- [GoWiki: 프로그래밍 문서 - 파일 읽기](https://github.com/golang/go/wiki/ProgrammingWiki#Reading_files)
- [Go Tour: 파일 입출력](https://go-tour-kr.appspot.com/#52)