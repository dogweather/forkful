---
title:                "Go: 미래나 과거의 날짜 계산하기"
simple_title:         "미래나 과거의 날짜 계산하기"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜

날짜를 미래 또는 과거로 계산하는 것에 대해 관심이 있을 때가 있습니다. 예를 들어, 특정 날짜에 어떤 일이 발생할 지 궁금하거나, 어떤 날짜가 특정 요일이라서 휴일이나 이벤트를 계획하고 싶을 때가 있을 수 있습니다.

## 어떻게

가장 간단한 방법은 time 패키지의 Now() 함수를 사용하여 현재 시간을 가져와서 원하는 날짜를 계산하는 것입니다. 예를 들어, 다음과 같이 코드를 작성할 수 있습니다:

```Go
day := time.Now().Date() // 현재 일자 가져오기
year := day.Year() // 현재 연도 가져오기
month := day.Month() // 현재 월 가져오기
dayOfMonth := day.Day() // 현재 날짜 가져오기

// 미래로 10일 뒤의 날짜 계산
future := time.Date(year, month, dayOfMonth+10, 0, 0, 0, 0, time.Local)

// 과거로 1년 전의 날짜 계산
past := time.Date(year-1, month, dayOfMonth, 0, 0, 0, 0, time.Local)

// 계산된 날짜 출력
fmt.Println("10일 후 날짜:", future.Format("2006-01-02"))
fmt.Println("1년 전 날짜:", past.Format("2006-01-02"))
```

위 코드의 결과는 다음과 같습니다:

```
10일 후 날짜: 2021-06-18
1년 전 날짜: 2020-06-08
```

## 깊게 알아보기

Go의 time 패키지는 더 복잡한 날짜 계산을 제공합니다. 예를 들어, 원하는 날짜의 요일을 가져오는 것이나 특정 날짜가 어떤 기간에 속하는지 확인하는 것이 가능합니다. 또한, 날짜와 시간을 형식화하거나 시간대를 설정하는 것도 가능합니다.

더 자세한 정보는 Go의 공식 문서에서 확인할 수 있습니다.

## 참고 자료

- [Go 공식 문서 - time 패키지](https://golang.org/pkg/time/)
- [time 패키지 더 알아보기 - GopherAcademy](https://blog.gopheracademy.com/advent-2013/day-08-go-dates/)
- [Go 언어의 시간과 날짜 처리 - 씨앗섬 기술블로그](https://blog.craftyworks.co.kr/66)
- [날짜 형식 지정 문자열 - Cheat Sheet](https://yourbasic.org/golang/time-format-parse-string-example/)