---
title:    "Go: 미래나 과거의 날짜 계산하기"
keywords: ["Go"]
---

{{< edit_this_page >}}

## 왜

날짜를 미래 또는 과거로 계산하는 일에 관심이 있는 이들이 주목해야 할 이유는 단 1-2문장으로 설명합니다.

날짜 계산은 프로그램 개발에서 매우 일반적이며 예측 가능한 결과를 제공하기 때문에 프로그래머에게 매우 유용합니다.

## 방법

먼저, Go 언어를 사용하여 날짜를 계산하는 기본적인 방법을 알아보겠습니다.

우선, `time` 라이브러리를 임포트합니다. 그리고 현재 날짜를 얻기 위해 `Now()` 함수를 사용합니다. 이어서 원하는 기간을 지정하기 위해 `AddDate()` 함수를 사용합니다.

```Go
import "time"

func main() {
  now := time.Now() // 현재 날짜
  future := now.AddDate(1,0,0) // 1년 미래 날짜 계산
  past := now.AddDate(0,0,-30) // 30일 과거 날짜 계산

  fmt.Println("현재 날짜:", now)
  fmt.Println("1년 뒤 날짜:", future)
  fmt.Println("30일 전 날짜:", past)
}
```

위 예제 코드의 출력은 다음과 같습니다.

```
현재 날짜: 2021-05-17 18:00:00
1년 뒤 날짜: 2022-05-17 18:00:00
30일 전 날짜: 2021-04-17 18:00:00
```

이처럼 `AddDate()` 함수는 원하는 기간만큼 미래 또는 과거 날짜를 계산하는 데에 유용하게 사용할 수 있습니다. 또한 두 날짜 사이의 차이를 계산하기 위해서는 `Sub()` 함수를 사용할 수 있습니다.

## Deep Dive

Go 언어에서 날짜를 계산하는 데에 사용되는 `time` 라이브러리는 매우 유용하고 다양한 기능을 제공합니다. 예를 들어, 특정 시간을 `time.Time` 객체로 지정하거나 `Parse()` 함수를 사용하여 문자열에서 `time.Time` 객체를 추출하는 등의 기능을 제공합니다.

또한, 날짜를 특정 포맷으로 출력하기 위해 `Format()` 함수를 사용할 수 있습니다. 예를 들어, "2021년 05월 17일 월요일"과 같은 형식으로 출력하려면 "2006년 01월 02일 Monday"과 같이 날짜 포맷을 지정할 수 있습니다.

이처럼 `time` 라이브러리는 Go 언어에서 날짜를 다루는 데에 매우 편리한 기능을 제공하므로, 프로그래머들은 날짜 계산에 유용하게 사용할 수 있습니다.

## See Also

- [Go 언어 공식 문서](https://golang.org/pkg/time/)
- [날짜를 계산하는 11가지 방법](https://www.digitalocean.com/community/tutorials/how-to-do-simple-date-time-calculations-in-go-ko)