---
title:    "Go: 두 날짜 비교하기"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜

많은 프로그래밍 언어에서 날짜를 비교하는 것은 쉬운 일이 아닙니다. 그러나 Go 언어에서는 비교하기 쉽고 강력한 내장 함수들이 있습니다. 이 글에서는 Go 언어를 사용해 두 날짜를 비교하는 방법에 대해 알아보겠습니다.

## 어떻게

Go 언어에서는 `time` 패키지를 사용하여 날짜와 시간을 다룹니다. 먼저 비교할 두 날짜를 `time.Time` 타입으로 만들어야 합니다. 그런 다음 `After()` 함수를 사용하여 두 날짜를 비교할 수 있습니다.

```
Go 코드 예시:

import "fmt"
import "time"

func main() {
    time1 := time.Date(2020, time.January, 1, 0, 0, 0, 0, time.UTC)
    time2 := time.Date(2019, time.December, 31, 0, 0, 0, 0, time.UTC)

    fmt.Printf("time1이 time2보다 이후인가요? %t\n", time1.After(time2))
}
```

```
코드의 출력:

time1이 time2보다 이후인가요? true
```

## 깊이 있는 탐구

`After()` 함수는 두 날짜 중 첫 번째 날짜가 두 번째 날짜보다 이후인지를 판단하는데 사용됩니다. 그렇다면 두 날짜 중 어느 하나도 이전일 경우에는 어떻게 할까요? 이럴 때는 `Before()` 함수를 사용하면 됩니다. 또한 두 날짜가 같은지 비교하는 `Equal()` 함수도 있습니다. 

그렇다면 시간을 포함해서 날짜와 시간을 비교하려면 어떻게 해야 할까요? 이 경우 `time.Time` 타입 대신 `time.Duration` 타입을 사용하여 두 시간의 차이를 계산하고 비교해야 합니다. 

## 자세한 정보

- [Go 언어 공식 문서 - time 패키지](https://golang.org/pkg/time)
- [날짜와 시간 처리를 위한 Go 언어 팁](https://blog.rubyranger.com/golang-datetime-tips)

----

## 관련 링크

- [Go 언어 공식 문서](https://golang.org/)
- [왜 Go 언어를 배워야 할까요?](https://www.lightsailblog.com/why-learn-go/)