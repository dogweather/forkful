---
title:                "테스트 작성하기"
html_title:           "Arduino: 테스트 작성하기"
simple_title:         "테스트 작성하기"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)

테스트 코드 작성은 코드가 의도한 대로 작동하는지 검증하는 과정입니다. 버그를 줄이고, 리팩토링을 안전하게 하며, 코드 품질을 확보하기 위해 프로그래머들이 테스트를 작성합니다.

## How to: (방법)

Go의 테스트는 간단하며 `testing` 패키지를 사용합니다. `_test.go` 파일에 단위 테스트를 만들어 보세요.

```go
package sum

import (
	"testing"
)

func Sum(a, b int) int {
	return a + b
}

func TestSum(t *testing.T) {
	total := Sum(5, 5)
	if total != 10 {
		t.Errorf("Sum was incorrect, got: %d, want: %d.", total, 10)
	}
}
```

테스트 실행:

```shell
$ go test
```

출력:

```
PASS
ok  	package/sum	0.002s
```

## Deep Dive (심층 분석)

테스트는 SUnit이라는 Smalltalk 테스팅 프레임워크에서 유래했습니다. Go의 테스트 툴은 `go test` 명령으로 간단하게 실행할 수 있으며, TDD(Test-Driven Development)와 같은 개발 방법론에 적합합니다. `testing` 패키지 외에도 `testify` 같은 서드파티 라이브러리가 있어 다양한 기능을 제공합니다.

## See Also (참고 자료)

- Go 테스트 공식 문서: [https://pkg.go.dev/testing](https://pkg.go.dev/testing)
- Go by Example 테스트 섹션: [https://gobyexample.com/testing](https://gobyexample.com/testing)
- Testify 라이브러리: [https://github.com/stretchr/testify](https://github.com/stretchr/testify)