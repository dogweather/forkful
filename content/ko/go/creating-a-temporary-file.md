---
title:    "Go: 임시 파일 만들기"
keywords: ["Go"]
---

{{< edit_this_page >}}

# 왜 만드는가

임시 파일을 만드는 이유는 프로그래밍에서 유용한 도구입니다. 임시 파일은 프로그램을 실행하는 동안 일시적으로 데이터를 저장하는 데 사용될 수 있습니다.

## 만드는 방법

Go에서 임시 파일을 만드는 방법은 간단합니다. ``` ioutil.TempFile()``` 함수를 사용하면 됩니다. 다음은 함수의 예제 코드입니다.

```Go
package main

import (
	"fmt"
	"io/ioutil"
)

func main() {
	file, err := ioutil.TempFile("", "prefix")
	if err != nil {
		fmt.Println("Error creating temporary file:", err)
		return
	}
	defer file.Close()

	fmt.Println("Temporary file created:", file.Name())
}
```

위 코드를 실행하면 다음과 같은 결과를 얻을 수 있습니다.

> Temporary file created: /tmp/prefix123456

해당 파일은 실행이 종료되면 자동으로 삭제됩니다.

## 깊이 파고들기

임시 파일이 삭제되는 것이 보장될 수 없는 경우, ```ioutil.TempDir()``` 함수를 사용하여 임시 디렉토리를 생성하고 여기에 파일을 저장할 수도 있습니다. 또한 임시 파일의 위치, 이름 및 확장자 등을 지정할 수 있습니다.

임시 파일은 대부분의 운영 체제에서 "시스템 전역"임을 명심해야 합니다. 따라서 프로그램이 임시 파일을 만들고 사용하는 동안 다른 프로그램이 해당 파일을 조작하거나 삭제할 수도 있습니다. 이러한 이유로 임시 파일의 보안과 관련된 이슈를 잊지 마세요.

## 관련 자료

- https://golang.org/pkg/io/ioutil/#TempFile
- https://golang.org/pkg/io/ioutil/#TempDir
- https://docs.microsoft.com/en-us/windows/win32/fileio/file-operations#temporary-file-operations
- https://www.linux.com/news/how-create-temporary-files-go/
- https://www.digitalocean.com/community/tutorials/how-to-create-temporary-files-and-directories-in-go