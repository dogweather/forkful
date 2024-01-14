---
title:    "Go: 디렉터리의 존재 여부 확인하기"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜
프로그래머의 일생은 결코 쉬운 일이 아닙니다. 많은 코드들이 있고 기능도 매우 다양합니다. 따라서 디렉토리가 실제로 존재하는지 아닌지 확인하는 것은 중요한 스킬 중 하나입니다.

## 어떻게
디렉토리의 존재 여부를 확인하기 위해서는 os 패키지의 `Stat` 함수를 사용해야 합니다. 이 함수는 파일 또는 디렉토리의 정보를 리턴해줍니다. 따라서 `Stat` 함수를 사용하여 디렉토리의 존재 여부를 판별할 수 있습니다.

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	// 디렉토리 경로
	dirPath := "directory_path"

	// Stat 함수를 사용하여 디렉토리 정보를 받아옴
	fileInfo, err := os.Stat(dirPath)
	if err != nil {
		// 에러 발생 시 디렉토리가 존재하지 않는 것으로 간주
		fmt.Println("디렉토리가 존재하지 않습니다.")
	} else {
		// 에러 없이 디렉토리 정보를 받아왔다면 해당 디렉토리가 존재하는 것으로 간주
		fmt.Println("디렉토리가 존재합니다.")
	}
}
```

위 코드의 실행결과는 다음과 같습니다.

```
디렉토리가 존재합니다.
```

## Deep Dive
프로그래밍에서 디렉토리를 다루는 것은 중요한 부분입니다. 디렉토리의 존재 여부를 확인하려면 파일 또는 디렉토리의 정보를 받아와야 하는데, 그 중에서도 가장 많이 사용되는 함수는 `Stat` 함수입니다. `Stat` 함수는 파일 또는 디렉토리의 정보를 리턴해주며, 디렉토리의 존재 여부를 확인할 수 있도록 에러 여부까지 확인할 수 있습니다.

## See Also
- Go 언어 공식 문서 - [os 패키지](https://golang.org/pkg/os/)
- Go 언어 공식 문서 - [Stat 함수](https://golang.org/pkg/os/#Stat)