---
title:    "Go: 테스트 작성 (Te-seu-teu jak-saeng)"
keywords: ["Go"]
---

{{< edit_this_page >}}

## 왜?

이글을 통해 테스트를 작성하는 이유를 배워볼까요? 테스트는 소프트웨어를 개발하는 과정에서 중요한 부분입니다. 테스트를 작성하면 코드가 정확하고 안정적인지 확인할 수 있고, 버그를 예방하고 수정하는 데 도움이 됩니다.

## 어떻게?

우리는 Go 언어로 테스트를 작성하는 방법을 배우겠습니다. 먼저, `testing` 패키지를 import 해야 합니다. 그리고 `testing.T`를 매개변수로 하는 테스트 함수를 작성합니다.

```Go
import "testing"

func TestAdd(t *testing.T) {
    result := add(1, 2)
    if result != 3 {
        t.Errorf("add(1, 2) = %d; want 3", result)
    }
}
```

위의 코드는 `add` 함수가 1과 2를 더했을 때 3이 나오는지를 테스트하는 예시입니다. 테스트할 함수에서 원하는 결과가 나오는지 확인하는 `if` 문을 작성합니다. 만약 실패할 경우, `t.Errorf` 함수를 사용하여 오류 메시지를 출력합니다.

## 깊히 들어가보기

테스트를 작성할 때에는 다양한 기법을 사용하여 테스트를 보다 간편하고 효율적으로 작성할 수 있습니다. 예를 들어, `testing.T` 인터페이스에 있는 `t.Helper()` 메소드를 사용하면 디버깅 정보를 더 자세히 볼 수 있고, 코드를 더 잘 읽을 수 있게 됩니다. 또한 `testing.M` 인터페이스를 사용하면 테스트 함수가 실패하는 경우에도 다음 테스트를 실행할 수 있습니다.

## 관련 링크들

### Go 언어 공식 문서
- https://golang.org/doc/

### Go 언어 테스트 패키지 문서
- https://golang.org/pkg/testing/

### 테스트 가능한 코드 작성하기
- https://golang.org/doc/tutorial/intro

## 함께보기

위에서 배운 것들에 대해 더 깊이 알아볼 수 있는 다른 자료들입니다.

- [Go 언어 테스트의 모범 사례](https://blog.golang.org/cover)
- [Go 언어와 테스트 기법](https://golang.org/blog/cover-test)
- [테스트 주도 개발](https://en.wikipedia.org/wiki/Test-driven_development)의 원리에 대한 정보