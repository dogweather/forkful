---
title:                "Go: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜

날짜를 비교하는 것은 Go 프로그래밍에서 매우 유용합니다. 예를 들어, 특정 날짜를 기준으로 작업을 계획하거나 어떤 날짜가 더 이전인지 비교하기 위해서 사용할 수 있습니다.

## 방법

날짜를 비교하는 가장 간단한 방법은 Go의 내장된 time 패키지를 사용하는 것입니다. 이 패키지에는 날짜를 비교할 수 있는 많은 함수들이 있습니다. 예를 들어, Equal 함수를 사용하여 두 날짜가 같은지 비교할 수 있습니다.

```
Go에서 두 날짜 비교 예제:

package main

import (
	"fmt"
	"time"
)

func main() {
	date1 := time.Date(2021, time.March, 8, 0, 0, 0, 0, time.Local)
	date2 := time.Date(2021, time.March, 9, 0, 0, 0, 0, time.Local)

	if date1.Equal(date2) {
		fmt.Println("두 날짜는 같습니다.")
	} else if date1.Before(date2) {
		fmt.Println("첫 번째 날짜가 두 번째 날짜보다 빠릅니다.")
	} else if date2.After(date1) {
		fmt.Println("두 번째 날짜가 첫 번째 날짜보다 늦습니다.")
	}
}

```

출력:

```
첫 번째 날짜가 두 번째 날짜보다 빠릅니다.
```

## 딥 다이브

날짜를 비교하는 더 깊은 정보를 알고 싶다면 Go의 time 패키지의 documentation을 살펴볼 수 있습니다. 이 문서에는 많은 예제와 함수들이 나와있어서 더 복잡한 날짜 비교를 할 수 있게 도와줍니다.

## 참고

- [Go 내장 time 패키지 문서](https://golang.org/pkg/time/)
- [Go time 패키지 관련 예제](https://stackoverflow.com/questions/62379163/how-can-i-compare-two-date-in-golang)