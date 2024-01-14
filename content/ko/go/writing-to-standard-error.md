---
title:                "Go: 표준 에러에 쓰는 방법"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜
모든 프로그래머들은 오류를 쉽게 찾을 수 있도록 로깅(logging)을 배우는 것이 중요합니다. 이 글에서는 로깅의 한 종류인 표준 에러(standard error)에 대해 알아보겠습니다.

## 방법
표준 에러에 쓰기 위해선 `os.Stderr`을 사용합니다. 다음은 Go 언어에서 표준 에러에 쓰는 예시 코드입니다:
```Go
import (
    "fmt"
    "os"
)

func main() {
    fmt.Fprintln(os.Stderr, "에러 메시지")
}
```

위 예시 코드는 `에러 메시지`를 표준 에러에 쓰게 됩니다. 이제 이 코드를 실행해보면 다음과 같은 출력을 볼 수 있습니다:
```
에러 메시지
```

## 깊이 파고들기
표준 에러에 쓰는 것은 오류를 디버깅할 때 매우 유용합니다. 하지만 주의해야 할 점이 있습니다. 표준 에러는 오로지 오류 메시지만 출력되는 것이 아니라, 다른 정보들도 함께 출력될 수 있습니다. 따라서 표준 에러를 사용할 때에는 적절한 정보만 출력되도록 조심해야 합니다.

## 참고 자료
- [Go 언어 공식 문서](https://golang.org/pkg/os/#Stderr)
- [표준 에러와 표준 출력의 차이](https://stackoverflow.com/questions/2737344/difference-between-stdout-and-stderr-in-c)
- [표준 에러를 사용할 때 주의할 점](https://blog.scottlowe.org/2015/01/02/understanding-stderr/)
- [표준 에러를 사용한 로깅 예제](https://www.ardanlabs.com/blog/2018/03/logging-in-go-part-three.html)

## 참고 자료