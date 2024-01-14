---
title:    "Go: 두 날짜 비교하기"
keywords: ["Go"]
---

{{< edit_this_page >}}

## 왜

날짜를 비교하는 것은 프로그래밍에서 매우 중요한 기능 중 하나입니다. 두 날짜를 비교하여 더 많은 가치있는 정보를 얻을 수 있으며, 예를 들어 이벤트의 기간을 계산하거나 기간 내에 특정 작업을 수행하는 등 다양한 용도로 사용할 수 있습니다.

## 사용 방법

두 날짜를 비교하는 가장 간단하고 흔한 방법은 `time` 패키지의 `Equal()` 함수를 사용하는 것입니다. 다음은 Go 언어로 작성된 예제 코드입니다.

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// 비교할 날짜 생성
	date1 := time.Date(2020, 1, 1, 0, 0, 0, 0, time.UTC)
	date2 := time.Date(2021, 1, 1, 0, 0, 0, 0, time.UTC)

	// 두 날짜를 비교하여 같은지 확인
	if date1.Equal(date2) {
		fmt.Println("두 날짜는 같습니다.")
	} else {
		fmt.Println("두 날짜는 다릅니다.")
	}
}
```

이 코드는 `두 날짜는 다릅니다.`라는 결과를 출력합니다. 즉, `date1`과 `date2`는 서로 다른 날짜를 나타내므로 `Equal()` 함수가 `false`를 반환합니다. 이제 `After()`와 `Before()` 함수를 사용하여 두 날짜의 상대적인 크기를 확인할 수 있습니다.

```Go
// date1이 date2보다 미래인지 확인
if date1.After(date2) {
	fmt.Println("date1이 date2보다 미래입니다.")
}

// date1이 date2보다 과거인지 확인
if date1.Before(date2) {
	fmt.Println("date1이 date2보다 과거입니다.")
}
```

출력:

```
date1이 date2보다 과거입니다.
```

이 외에도 비교 연산자를 사용하여 두 날짜의 크기를 비교할 수 있습니다. 자세한 내용은 Go 언어 공식 문서를 참조하시기 바랍니다.

## 깊이 들어가기

날짜를 비교하는 또 다른 방법은 `Sub()` 함수를 사용하는 것입니다. 이 함수는 두 날짜 간의 시간 차이를 나타내는 `Duration` 타입의 값을 반환합니다. 아래 코드를 살펴보겠습니다.

```Go
// time.Now() 함수는 현재 시간을 나타내는 값을 반환
now := time.Now()

// 2021년 1월 1일부터 현재까지 시간 차이 계산
diff := now.Sub(time.Date(2021, 1, 1, 0, 0, 0, 0, time.UTC))

fmt.Println("과거부터 현재까지 시간 차이:", diff)
```

출력:

```
과거부터 현재까지 시간 차이: 408h22m53.7132815s
```

위 코드에서 `diff` 변수는 `Duration` 타입이므로 시간, 분, 초 등을 각각 따로 알고 싶을 경우 `Duration` 타입의 `Hours()`, `Minutes()`, `Seconds()` 등의 메소드를 사용할 수 있습니다.

## 또 다른 자료

- [Go 언어 공식 문서: time 패키지](https://golang.org/pkg/time/)
- [Go 언어 공식 문서: Duration 타입](https://golang.org/pkg/time/#Duration)
- [A Tour of Go: 날짜와 시간](https://tour.golang.org/welcome/15)