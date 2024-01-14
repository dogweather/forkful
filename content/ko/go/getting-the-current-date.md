---
title:                "Go: 현재 날짜 얻기"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜

Go 프로그래밍을 위해 현재 날짜를 얻는 것에 대해 궁금할 수 있습니다. 알아보는 것에 관심이 있다면, 이 글을 읽어보세요.

## 어떻게

Go에서 현재 날짜를 얻는 것은 매우 쉽습니다. 단지 `time.Now()` 함수를 호출하면 됩니다. 아래의 코드를 보세요:

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    now := time.Now()
    fmt.Println("오늘의 날짜는:", now)
}
```

위 코드를 실행하면 다음과 같은 출력이 나타납니다:

```
오늘의 날짜는: 2020-05-19 13:46:00.1565351 +0900 KST m=+0.003985101
```

위 예시에서 처럼, `time.Now()` 함수는 현재 시간뿐만 아니라 날짜와 시간의 모든 정보를 반환합니다. 따라서 원하는대로 날짜만을 사용하려면 `now.Date()` 메소드를 사용하면 됩니다. 아래의 코드를 참고하세요:

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    now := time.Now()
    fmt.Println("오늘의 날짜는:", now.Date())
}
```

위 코드를 실행하면 다음과 같은 출력이 나타납니다:

```
오늘의 날짜는: 2020 May 19
```

## 깊이 파고들기

Go의 `time` 패키지에는 날짜 및 시간을 다루는 다양한 기능이 포함되어 있습니다. 이 중에는 시간대 변환, 날짜 계산 등 유용한 기능들이 있습니다. 자세한 내용은 공식 문서를 참고하시길 바랍니다.

## 또 다른 자료들

- Go 공식 문서: https://golang.org/pkg/time/