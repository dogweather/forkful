---
title:                "임시 파일 생성하기"
html_title:           "Python: 임시 파일 생성하기"
simple_title:         "임시 파일 생성하기"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 무엇과 왜?

임시 파일 생성은 사용자의 데이터가 일시적으로 저장되는 파일을 만드는 것입니다. 프로그래머들은 디버깅이나 데이터를 일시적으로 저장하고 추후에 사용하기 위해 이를 사용합니다.

## 동작 방식:

임시 파일 생성은 Go의 `io/ioutil` 패키지에 `TempFile` 함수를 사용하여 수행할 수 있습니다. 다음은 간단한 예입니다.

```Go
package main

import (
	"fmt"
	"io/ioutil"
	"log"
)

func main() {
	tempFile, err := ioutil.TempFile("", "sample")
	if err != nil {
		log.Fatal(err)
	}

	fmt.Println(tempFile.Name())

	// Use the file
}
```
이 샘플 코드는 임시 디렉토리에 'sample*' 이름을 가진 임시 파일을 생성합니다.
출력:

```Go
/tmp/sample437309879
```

## 딥 다이브:

임시 파일 생성은 오래전부터 사용되어 왔습니다. 하지만, 이는 프로그래밍 컨텍스트 내에서 항상 최선의 선택이 아닙니다. 데이터베이스 또는 키-값 저장소 같은 다른 매체들이 더 적합할 수 있습니다. 게다가, Go의 `ioutil.TempFile` 함수는 보안에 중점을 둔 디자인을 갖추고 있습니다. 즉, 동일한 이름을 가진 임시 파일을 만드는 경쟁 조건은 발생하지 않습니다.

## 참고:

- Go 공식 문서: [ioutil.TempFile](https://golang.org/pkg/io/ioutil/#TempFile)
- "Creating temporary files in Go": (https://www.calhoun.io/creating-temporary-files-in-go/)