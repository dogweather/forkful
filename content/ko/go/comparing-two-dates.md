---
title:                "Go: 두 개의 날짜 비교"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜

두 날짜를 비교하는 것이 왜 중요한지에 대해 궁금한 적이 있나요? Go 프로그래밍 언어에서 두 날짜를 비교하는 것은 특정 이벤트의 발생 시간을 파악하거나 조건문을 실행하는 데 유용합니다. 또한 개발자가 프로그램 내에서 날짜와 관련된 데이터를 다룰 때 필수적인 과정입니다.

## 하는 방법

Go 언어에서 두 날짜를 비교하는 방법을 알고 싶다면 아래의 코드 예제를 참고해주세요.

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    // 비교할 두 날짜 생성
    time1 := time.Date(2020, time.April, 20, 12, 0, 0, 0, time.UTC)
    time2 := time.Date(2020, time.April, 30, 12, 0, 0, 0, time.UTC)

    // Before 함수를 사용해 time1이 time2보다 이전인지 확인
    if time1.Before(time2) {
        fmt.Println(time1, "is before", time2)
    }

    // After 함수를 사용해 time1이 time2보다 이후인지 확인
    if time2.After(time1) {
        fmt.Println(time2, "is after", time1)
    }

    // Equal 함수를 사용해 두 날짜가 동일한지 확인
    if time1.Equal(time2) {
        fmt.Println(time1, "and", time2, "are equal")
    }
}
```
출력:
```
2020-04-20 12:00:00 +0000 UTC is before 2020-04-30 12:00:00 +0000 UTC
2020-04-30 12:00:00 +0000 UTC is after 2020-04-20 12:00:00 +0000 UTC
```

## 자세히 알아보기

Go 언어에서는 날짜와 시간을 다루기 위해 time 패키지를 제공합니다. 이 패키지에는 날짜 비교를 위해 사용할 수 있는 다양한 함수들이 있습니다. 앞서 살펴본 Before, After, Equal 함수 외에도 두 날짜의 차이를 구하는 함수인 Sub, 년, 월, 일, 시간 등의 정보를 추출할 수 있는 함수들이 있습니다.

이 외에도 날짜 형식을 다루는 방법 등 더 깊이 들어가고 싶다면 Go 언어 공식 문서를 참고하는 것을 추천합니다.

## 더 보기

- [Go 언어 공식 문서](https://golang.org/doc/)
- [날짜와 시간 다루기 - GolangCode](https://golangcode.com/handle-dates-and-times/)
- [기본 Go 문법 - TUG](https://tutorialedge.net/golang/go-tutorial-basics/)
- [웹 개발에 이용되는 Go 언어 - TDD](https://thewebdev.info/2020/05/08/go-web-development-tutorial-goroutines-channels-pool-routings-views-middlewares-select-gonic-gorm/)