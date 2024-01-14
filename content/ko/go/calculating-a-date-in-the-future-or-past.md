---
title:                "Go: 미래 또는 과거에 날짜 계산하기"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜

날짜를 미래 또는 과거로 계산하는 것은 프로그래머가 시간과 날짜를 다루는 것이 필요하기 때문입니다.

## 사용 방법

날짜를 미래나 과거로 계산하는 방법은 Go 언어 기능을 활용하는 것입니다. 아래는 예시 코드와 결과 값입니다.

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// 현재 날짜
	today := time.Now()

	// 미래로 7일 계산
	future := today.AddDate(0, 0, 7)

	// 과거로 1월 계산
	past := today.AddDate(0, -1, 0)

	// 결과 출력
	fmt.Println("현재 날짜:", today)
	fmt.Println("미래 날짜:", future)
	fmt.Println("과거 날짜:", past)
}
```

결과:

```bash
현재 날짜: 2021-03-25 14:26:30.610374 +0900 KST m=+0.000847026
미래 날짜: 2021-04-01 14:26:30.610374 +0900 KST m=+604800.000847026
과거 날짜: 2021-02-25 14:26:30.610374 +0900 KST m=+2628932.000847026
```

## 깊이있는 알아보기

Go 언어에서 시간과 날짜를 다루는 데에 사용되는 기능 중 하나는 `AddDate()` 함수입니다. 이 함수는 `int`형 변수를 인수로 받아 날짜를 미래나 과거로 계산합니다. `AddDate()` 함수 뒤에는 `Add()` 함수를 사용해 시간 단위까지 조절할 수 있습니다. 더 많은 정보를 알고 싶다면 공식 Go 언어 문서를 참고하세요.

## 관련 링크

- `AddDate()` 함수 공식 문서: https://golang.org/pkg/time/#AddDate
- Go 언어 시간과 날짜 다루기: https://velog.io/@jyoo0523/Go 
- Go 언어 기초: https://www.44bits.io/ko/post/go-basic-concept 
- Go 언어 공식 홈페이지: https://golang.org/
- Go 언어 공식 블로그: https://blog.golang.org/