---
title:                "현재 날짜 가져오기"
date:                  2024-01-20T15:15:18.774984-07:00
html_title:           "Bash: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)

시스템의 현재 날짜와 시간을 얻는 것은 기본적이지만 필수적인 기능입니다. 로깅, 타임스탬프, 사용자 이벤트 기록 등 다양한 목적으로 프로그래머들은 이를 사용합니다.

## How to: (방법)

Go에서 현재 날짜와 시간을 얻으려면 `time` 패키지를 사용하세요.

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    now := time.Now()
    fmt.Println("현재 날짜와 시간: ", now)
}
```

Sample Output:

```
현재 날짜와 시간: 2023-04-05 17:45:42.371625 +0000 UTC
```

## Deep Dive (심화 탐구)

컴퓨터의 시간은 일반적으로 표준 시간대에 맞춰지며, UTC (협정 세계시)를 기준으로 합니다. `time` 패키지는 2009년에 Go의 초창기 버전에 도입되었으며, 타임존과 시간 포맷을 다룰 수 있는 강력한 기능을 제공합니다. 

UTC와의 차이를 고려해서 로컬 타임을 얻거나 타임존에 맞는 시간을 설정할 수도 있습니다. 예를 들어, 서울의 경우 `Asia/Seoul` 타임존을 사용합니다.

```Go
seoul, _ := time.LoadLocation("Asia/Seoul")
nowInSeoul := time.Now().In(seoul)
fmt.Println("서울 현재 시간: ", nowInSeoul)
```

Sample Output:

```
서울 현재 시간: 2023-04-06 02:45:42.371625 +0900 KST
```

스탠다드 라이브러리 외에도, 일부 서드파티 라이브러리들이 더 복잡한 날짜와 시간 조작을 위해 사용할 수 있지만, 표준 `time` 패키지는 대부분의 요구를 만족시킵니다.

## See Also (관련 자료)

- [Go `time` Package Documentation](https://pkg.go.dev/time)
- [Go by Example: Time](https://gobyexample.com/time)
- [The Go Programming Language Specification](https://golang.org/ref/spec#Time_and_duration_types)
