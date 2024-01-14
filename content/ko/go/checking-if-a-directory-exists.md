---
title:                "Go: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜?
어떤 디렉토리가 존재하는지 확인하는 것은 프로그래머에게 중요한 일입니다. 디렉토리가 존재하는지 여부를 미리 확인함으로써 프로그램의 안전성을 보장할 수 있습니다.

## 방법
```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	// 존재하는 디렉토리 경로를 지정합니다.
	path := "C:\\Users\\User\\Documents\\Go\\test"

	// 디렉토리의 존재 여부를 확인합니다.
	if _, err := os.Stat(path); err == nil {
		// 디렉토리가 존재하는 경우
		fmt.Println("디렉토리가 존재합니다.")
	} else {
		// 디렉토리가 존재하지 않는 경우
		fmt.Println("디렉토리가 존재하지 않습니다.")
	}
}
```
```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	// 존재하지 않는 디렉토리 경로를 지정합니다.
	path := "C:\\Users\\User\\Documents\\Go\\test2"

	// 디렉토리의 존재 여부를 확인합니다.
	if _, err := os.Stat(path); err == nil {
		// 디렉토리가 존재하는 경우
		fmt.Println("디렉토리가 존재합니다.")
	} else {
		// 디렉토리가 존재하지 않는 경우
		fmt.Println("디렉토리가 존재하지 않습니다.")
	}
}
```

위 코드를 실행하면 존재하는 디렉토리와 존재하지 않는 디렉토리 두 가지를 확인할 수 있습니다. 디렉토리가 존재하는 경우에는 "디렉토리가 존재합니다."라는 메시지가 출력되고, 존재하지 않는 경우에는 "디렉토리가 존재하지 않습니다."라는 메시지가 출력됩니다.

## 깊이 파고들기
Go 언어에서 디렉토리의 존재 여부를 알아내는 가장 간단한 방법은 os 패키지의 Stat 함수를 사용하는 것입니다. Stat 함수를 사용하면 해당 파일이나 디렉토리의 정보를 가져올 수 있고, 존재 여부를 확인할 수 있습니다. os 패키지에는 더 많은 함수들이 존재하므로 여러분의 프로그램에 맞는 가장 적합한 방법을 선택할 수 있습니다.

## 참고자료
- https://golang.org/pkg/os/#Stat
- https://zetawiki.com/wiki/Go_%EA%B0%9D%EC%B2%B4_%EC%A1%B0%ED%9A%8C_%EC%97%AC%EB%B6%80_%EB%B3%80%EC%88%98_%EA%B5%AC%ED%95%98%EA%B8%B0