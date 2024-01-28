---
title:                "에러 처리하기"
date:                  2024-01-26T00:53:35.424129-07:00
model:                 gpt-4-1106-preview
simple_title:         "에러 처리하기"
programming_language: "Go"
category:             "Go"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/handling-errors.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

Go에서의 에러 처리는 런타임 문제를 우아하게 잡아내고 대응하는 것입니다. 프로그램이 충돌하는 것을 방지하고, 문제가 생겨도 예측 가능하게 동작하도록 하기 위해 이를 수행합니다.

## 어떻게:

Go는 명시적인 에러 처리를 사용합니다. 이 말은 함수를 호출할 때마다 에러를 반환하는지 확인해야 함을 의미합니다. 예외 없음. 다음은 그 모습입니다:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	err := doSomething()
	if err != nil {
		fmt.Println("이런:", err)
		os.Exit(1)
	}
}

func doSomething() error {
	// 일부러 문제를 발생시키는 척함
	return fmt.Errorf("문제가 발생했습니다")
}
```

이것을 실행하면 다음을 얻게 됩니다:

```
이런: 문제가 발생했습니다
```

그런데 성공하면 어떨까요?

```Go
func doSomething() error {
	// 이번에는 모든 것이 잘 됨
	return nil
}
```

출력 없음. 좋습니다, 무소식이 희소식이라고 하죠.

## 심층 분석:

Go에서 에러 처리는 의견의 차이가 있는 주제입니다. 처음부터 Go는 예외를 사용하지 않고 보다 명시적인 접근 방식을 선택했는데, 일부 개발자들은 이의 단순함을 좋아하고 다른 일부는 장황하다고 생각합니다. 내장된 `error` 타입은 인터페이스입니다. `Error() string` 메소드가 있는 모든 타입은 그것을 만족시킵니다. 이는 Go의 단순성과 명시성에 대한 철학과 관련이 있습니다.

대안은? `panic`과 `recover` 쌍이 있지만, 이는 프로그램이 계속될 수 없을 때 예외적인 경우(의도한 말장난)에 사용됩니다. `panic`을 오는 길이 없다는 것을 알 때 누르는 탈출 버튼으로 생각하십시오. 아낌없이 사용하지 마십시오.

주류 에러 처리에 관하여, Go 1.13은 에러 래핑 기능을 도입하여 `errors.Is()`와 `errors.As()`와 같은 함수를 사용해 "에러 체인"을 더 쉽게 파악할 수 있게 만들었습니다.

## 또한 보기:

Go에서의 모든 에러 처리에 대하여:

- Go 블로그에서 에러 처리에 관하여: [https://blog.golang.org/error-handling-and-go](https://blog.golang.org/error-handling-and-go)
- 효과적인 Go – 에러 처리 절: [https://golang.org/doc/effective_go#errors](https://golang.org/doc/effective_go#errors)
- Go 1.13 에러 래핑 문서: [https://golang.org/doc/go1.13#error_wrapping](https://golang.org/doc/go1.13#error_wrapping)
- Dave Cheney의 에러 처리 전략에 관한 글: [https://dave.cheney.net/2016/04/27/dont-just-check-errors-handle-them-gracefully](https://dave.cheney.net/2016/04/27/dont-just-check-errors-handle-them-gracefully)
