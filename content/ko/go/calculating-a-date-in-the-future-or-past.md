---
title:                "미래나 과거의 날짜 계산하기"
date:                  2024-01-20T17:31:36.788434-07:00
model:                 gpt-4-1106-preview
simple_title:         "미래나 과거의 날짜 계산하기"

category:             "Go"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 무엇과 왜?
날짜 계산이란 현재로부터 과거나 미래의 날짜를 구하는 것을 말합니다. 프로그램 내에서 예약 시스템을 구축하거나 기한을 계산할 때 많이 사용합니다.

## 실행 방법:
Go에서 `time` 패키지를 사용하면 쉽게 날짜 계산을 할 수 있습니다.

```go
package main

import (
	"fmt"
	"time"
)

func main() {
	// 현재 시각
	now := time.Now()
	fmt.Println("지금 시각:", now)

	// 2주 뒤
	twoWeeksLater := now.Add(time.Hour * 24 * 14)
	fmt.Println("2주 후:", twoWeeksLater)

	// 3일 전
	threeDaysBefore := now.Add(-time.Hour * 24 * 3)
	fmt.Println("3일 전:", threeDaysBefore)
}
```
출력 예시:
```
지금 시각: 2023-04-12 15:04:05.999999 +0900 KST
2주 후: 2023-04-26 15:04:05.999999 +0900 KST
3일 전: 2023-04-09 15:04:05.999999 +0900 KST
```

## 심층 탐구:
날짜 계산은 율리우스 달력과 그레고리오 달력의 도입 이후로 중요했습니다. 그럼에도 불구하고 기계와 컴퓨터를 이용한 정확한 계산은 20세기에 들어서야 가능해졌어요. Go 언어에서는 `time` 패키지를 사용하여 시간과 날짜를 다룹니다. `Add` 메서드 외에도 `AddDate`를 사용해 년, 월, 일 단위로 계산할 수 있습니다. 표준 라이브러리 외에, 대안으로 `github.com/rickar/cal` 같은 타사 라이브러리를 사용해서 휴일이나 다양한 공휴일 처리를 할 수도 있습니다.

## 참고 자료:
- [Go 공식 문서의 time 패키지](https://golang.org/pkg/time/)
- [github.com/rickar/cal: Go 휴일 패키지](https://github.com/rickar/cal)
- [위키피디아의 "율리우스 달력"](https://ko.wikipedia.org/wiki/%EC%9C%A8%EB%A6%AC%EC%9A%B0%EC%8A%A4_%EB%8B%AC%EB%A0%A5)
