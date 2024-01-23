---
title:                "디버그 출력을 찍어보기"
date:                  2024-01-20T17:52:58.061026-07:00
model:                 gpt-4-1106-preview
simple_title:         "디버그 출력을 찍어보기"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가요? 왜 사용하나요?)
디버그 출력은 코드가 실행될 때 중간 과정이나 값들을 확인하는 것입니다. 프로그래머들은 버그 찾기, 코드 흐름 확인, 혹은 값 검증을 위해 사용합니다.

## How to: (어떻게 하나요?)
Go에선 `fmt` 패키지를 사용해 콘솔에 정보를 출력합니다. 다음은 간단한 예제 코드와 출력 결과입니다.

```Go
package main

import (
	"fmt"
)

func main() {
	debugMessage := "디버그 시작"
	fmt.Println(debugMessage) // 콘솔 출력 예제

	// 변수 값 확인
	a, b := 5, 10
	fmt.Printf("a: %d, b: %d\n", a, b) 
}
```

출력 결과:
```
디버그 시작
a: 5, b: 10
```

## Deep Dive (심층 분석)
디버깅은 프로그래밍 초기부터 개발 과정의 핵심 부분이었습니다. ‘디버그(debug)’라는 말은 1947년 하버드 대학의 마크 II 컴퓨터에서 실제 벌레를 찾아내어 문제를 해결했다는 유명한 일화에 기인합니다.

Go 언어에서는 `fmt` 패키지 말고도 `log` 패키지나 `os` 패키지를 통해서도 디버그 정보를 출력할 수 있습니다. 더 복잡한 상황에 주로 `log` 패키지를 사용할 수 있으며, 이는 시간이나 다른 상세한 정보를 함께 기록할 수 있는 기능을 제공합니다.

```Go
import (
	"log"
)

func main() {
	log.Println("디버그 로그 메시지")
}
```

또한, 표준 라이브러리가 아닌, `glog`나 `logrus` 같은 서드파티 라이브러리들을 이용하여 더 많은 기능과 유연함을 제공받을 수 있습니다.

## See Also (참고 자료)
- Go 언어 공식 문서의 fmt 패키지: https://pkg.go.dev/fmt
- Go 언어 공식 문서의 log 패키지: https://pkg.go.dev/log
- `logrus` 라이브러리: https://github.com/sirupsen/logrus
- `glog` 라이브러리: https://github.com/golang/glog
- Effective Go 문서에서의 로깅 관련 팁: https://golang.org/doc/effective_go#logging
