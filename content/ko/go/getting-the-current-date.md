---
title:                "현재 날짜 가져오기"
html_title:           "Go: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜

현재 날짜를 가져오는 것이 왜 필요할까요? 현재 날짜를 사용하면 다양한 프로그래밍 작업에서 유용하며, 시간 기반의 작업을 할 때 매우 중요합니다.

## 하우투

```Go
import "fmt"
import "time"

func main() {
  // 현재 시간에 대한 객체 생성
  now := time.Now()

  // 현재 시간에서 년, 월, 일, 시분초를 가져옴
  year := now.Year()
  month := now.Month()
  day := now.Day()
  hour := now.Hour()
  minute := now.Minute()
  second := now.Second()

  // 출력 예시: 2021-08-01 09:30:45
  fmt.Printf("%d-%02d-%02d %02d:%02d:%02d", year, month, day, hour, minute, second)
}
```

### 결과

```
2021-08-01 09:30:45
```

## 딥 다이브

Go 언어는 표준 라이브러리인 time 패키지를 통해 현재 시간을 쉽게 가져올 수 있습니다. 이 패키지는 익숙한 time 관련 작업을 간편하게 처리할 수 있도록 여러 함수와 형식을 제공합니다.

### 현재 시간 가져오기

지금까지 보여드린 방법은 time.Now()를 사용하는 방법이었습니다. 이 함수는 시간대 정보를 무시하고 현재 시간정보를 가지고 있는 Time 구조체를 반환합니다.

### 다른 시간대의 현재 시간 가져오기

위의 예제에서 사용한 time.Now() 함수는 로컬 시간 기준으로 현재 시간을 반환합니다. 하지만 시간대 정보를 고려한 현재 시간을 얻고 싶다면 다른 함수를 사용해야 합니다. 예를 들어, time.Now().UTC()는 세계 표준시 기준의 현재 시간을 반환합니다.

### 시간 형태 변경하기

Go 언어에서는 time 패키지의 Format 함수를 사용하여 시간을 원하는 형태로 변환할 수 있습니다. 위의 예제에서 시간을 "연-월-일 시분초" 형태로 출력하기 위해 Format 함수를 사용했습니다. 다른 형식도 가능하니 필요에 맞게 사용하면 됩니다.

## 관련 자료

- [Go 언어 공식 사이트](https://golang.org/)
- [time 패키지 문서](https://golang.org/pkg/time/)
- [Go 언어를 활용한 실전 예제](https://gobyexample.com/)