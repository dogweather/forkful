---
title:                "Go: 프로그래밍에서 테스트 작성하기"
simple_title:         "프로그래밍에서 테스트 작성하기"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/writing-tests.md"
---

{{< edit_this_page >}}

## 왜?

Go 프로그래밍에서 테스트를 작성하는 이유는 코드의 정확성과 안정성을 보장하기 위해서입니다. 테스트를 통해 버그를 예방하고 파일을 깔끔하게 유지할 수 있습니다.

## 어떻게?

테스트 작성하기

```Go
package main

import (
  "testing"
  "github.com/stretchr/testify/assert"
)

func TestSum(t *testing.T) {
  result := sum(2, 3)
  expected := 5
  assert.Equal(t, expected, result, "Sum() 함수 결과는 5여야 합니다.")
}

func TestSubtract(t *testing.T) {
  result := subtract(5, 2)
  expected := 3
  assert.Equal(t, expected, result, "Subtract() 함수 결과는 3이여야 합니다.")
}

func sum(a, b int) int {
  return a + b
}

func subtract(a, b int) int {
  return a - b
}
```

테스트 실행하기

```
go test
```

테스트 결과

```
PASS
ok      [패키지 경로]      0.007s
```

## 깊이 파고들기

테스트 작성에는 다양한 방법이 있습니다. 위의 예시는 아주 간단한 유닛 테스트이며, 더 복잡한 테스트에는 모의 객체와 테이블 테스트를 사용할 수 있습니다. 또한 코드의 매니저로서의 역할도 테스트를 작성할 때 고려해야 하는 중요한 부분입니다.

## 참고 자료

- [GoLang.org: Test](https://golang.org/pkg/testing/)
- [Medium: Go언어 테스트 코드 작성하기](https://medium.com/@daeseokhan88/go%EC%96%B8%EC%96%B4-%ED%85%8C%EC%8A%A4%ED%8A%B8-%EC%BD%94%EB%93%9C-%EC%9E%91%EC%84%B1%ED%95%98%EA%B8%B0-996069c7df82)
- [디렉토리 구조를 매니징하는 Go 프로젝트의 테스트 케이스 추가법](https://blog.ysmood.org/unit-testing-in-golang/)