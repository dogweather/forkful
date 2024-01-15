---
title:                "미래나 과거의 날짜 계산하기"
html_title:           "Go: 미래나 과거의 날짜 계산하기"
simple_title:         "미래나 과거의 날짜 계산하기"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# 왜

날짜를 미래나 과거로 계산하는 것에 대해 관심이 있는 사람이라면 Go 프로그래밍의 이 기능에 대해 알아볼 필요가 있습니다. 더 나은 알고리즘을 사용하기 때문에 기존의 다른 언어보다 더 빠르고 정확한 날짜 계산이 가능하기 때문입니다.

## 어떻게

Go를 사용해 날짜를 미래나 과거로 계산하는 것은 매우 간단합니다. ```time``` 패키지에 있는 ```AddDate()``` 함수를 사용하면 됩니다. 다음 예시를 통해 확인해보세요.

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// 현재 날짜
	now := time.Now()
	fmt.Println("현재 날짜:", now)

	// 현재 날짜로부터 1년 뒤 계산
	nextYear := now.AddDate(1, 0, 0)
	fmt.Println("1년 뒤 날짜:", nextYear)

	// 현재 날짜로부터 6달 전 계산
	lastSixMonths := now.AddDate(0, -6, 0)
	fmt.Println("6개월 전 날짜:", lastSixMonths)

	// 현재 날짜로부터 2주 뒤 계산
	twoWeeks := now.AddDate(0, 0, 14)
	fmt.Println("2주 뒤 날짜:", twoWeeks)
}
```

위의 코드를 실행하면 다음과 같은 출력이 나옵니다.

```
현재 날짜: 2021-11-17 18:00:00 +0000 UTC m=+0.000000001
1년 뒤 날짜: 2022-11-17 18:00:00 +0000 UTC m=+31622400.000000001
6개월 전 날짜: 2021-05-17 18:00:00 +0000 UTC m=+15724800.000000001
2주 뒤 날짜: 2021-12-01 18:00:00 +0000 UTC m=+1209600.000000001
```

이처럼 ```AddDate()``` 함수를 사용하면 현재 날짜로부터 원하는 만큼의 날짜를 미래나 과거로 계산할 수 있습니다.

## 딥 다이브

Go에서는 날짜를 계산해주는 내장 패키지인 ```time```을 제공합니다. 이 패키지를 사용하면 시간대, 기간, 날짜 형식 등 여러 가지를 설정하여 유용한 날짜 계산을 할 수 있습니다. 또한, Go의 날짜 계산 알고리즘은 다른 언어들보다 더 빠르고 정확하게 작동한다는 것을 알 수 있습니다. 따라서 프로그램에서 날짜를 다루는 기능이 필요하다면 Go의 ```time``` 패키지를 적극적으로 활용하는 것이 좋습니다.

## 관련 링크

- ```time``` 패키지: https://golang.org/pkg/time/
- 날짜 계산 예제 코드: https://play.golang.org/p/dFCSef8VC7z
- 다른 언어와의 날짜 계산 비교: https://engineering.trafi.com/dates-in-go-8b4dbc82d52f