---
title:                "두 날짜 비교하기"
html_title:           "Go: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜

어떤 사람이 두 날짜를 비교하는 것에 참여하는 이유는 무엇일까요? Go 언어를 사용하여 두 날짜를 비교하는 방법을 알아보고 더 깊이있게 알아보도록 하겠습니다.

## 어떻게

```Go
// 두 날짜를 비교하는 함수
func compareDates(date1 time.Time, date2 time.Time) int {
	// 부르는 함수에서 더 큰 날짜를 date1로 넘겨줌
	if date1.After(date2) {
		return 1 // date1이 date2보다 크면 1 반환
	} else if date1.Before(date2) {
		return -1 // date2가 date1보다 크면 -1 반환
	} else {
		return 0 // 두 날짜가 같으면 0 반환
	}
}

// 함수 호출 예시
date1 := time.Date(2021, time.January, 1, 0, 0, 0, 0, time.UTC)
date2 := time.Date(2021, time.April, 1, 0, 0, 0, 0, time.UTC)
result := compareDates(date1, date2)
fmt.Println(result)
// 결과: -1 (date1이 date2보다 작음)
```

## 깊게 알아보기

날짜를 비교하기 전에 알아야 할 개념은 "시간"과 "날짜"입니다. Go 언어에서는 시간을 다루기 위해 `time` 패키지를 사용하며, 이 패키지에는 `Time`이라는 타입이 존재합니다. `Time` 타입은 시간과 날짜를 모두 포함하며, `time.Now()` 함수를 사용하면 현재 시간을 가져올 수 있습니다.

날짜를 비교하기 위해 Go 언어에서는 `After()`와 `Before()` 함수를 제공합니다. `After()` 함수는 첫 번째 파라미터로 넘어온 `Time` 값이 두 번째 파라미터로 넘어온 `Time` 값보다 큰지를 비교하는 함수이며, `After()` 함수와 반대로 `Before()` 함수는 작은지를 비교합니다. 만약 두 날짜가 같으면 `After()`와 `Before()` 함수는 모두 `false`를 반환합니다.

현재 시간을 비교하는 `time.Now()` 함수와 `After()` 함수를 함께 사용하면 현재 시간이 이전인지 이후인지를 판단할 수 있습니다. 또한 두 시간 간의 차이를 구할 수도 있습니다. `Sub()` 함수를 사용하면 `Duration` 타입의 값으로 반환되며, `Hours()`, `Minutes()`, `Seconds()` 함수를 이용하여 시간 단위로 차이를 알 수 있습니다.

## 더 알아보기

- [Go 언어 공식 문서](https://golang.org/doc/)
- [Go 언어 날짜와 시간 다루기](https://zetawiki.com/wiki/Go_언어_날짜와_시간_다루기)
- [Effective Go - Time](https://golang.org/doc/effective_go.html#time)
- [Go 프로그래밍 - 날짜와 시간 다루기](https://go-tour-ko.appspot.com/basics/15)
- [Go 표준 라이브러리 - 날짜와 시간](https://golang.org/pkg/time/)

## 관련 링크

- [Go 언어를 사용하여 현재 날짜와 맞는 파일 이름 생성하기](https://velog.io/@hyeon930/Go-언어를-사용하여-현재-날짜와-맞는-파일-이름-생성하기)
- [Go 언어를 사용하여