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

## 현재 날짜 가져오기: 
오늘 날짜를 가져오는 것은 많은 프로그래머들에게 중요하다. 날짜는 소프트웨어에서 많은 역할을 하며, 예약 시스템이나 매출 추적 등에 필수적인 정보다. 따라서 현재 날짜를 빠르고 간편하게 가져오는 것은 매우 유용한 능력이다.

## 어떻게 하는가:
Go 언어에서는 time 패키지를 사용하여 현재 날짜와 시간을 가져올 수 있다. 아래의 코드 블록은 현재 날짜와 시간을 출력하는 예제이다.
```Go
import "fmt"
import "time"

func main() {
  now := time.Now()
  fmt.Println("현재 날짜와 시간: ", now)
}
```

출력 결과:
```sh
현재 날짜와 시간: 2021-07-20 14:30:00.000000 +0000 UTC m=+0.000000001
```

## 더 깊게 파헤치기:
날짜와 시간은 프로그래밍에서 중요한 부분이다. 예전에는 time 패키지 없이 직접 현재 날짜와 시간을 계산하거나 운영체제의 날짜와 시간 정보를 가져와서 사용했지만, 지금은 time 패키지를 사용하여 간편하고 정확하게 현재 날짜와 시간을 가져올 수 있다.

다른 언어에서도 비슷한 기능을 제공하는데, Python에서는 datetime 모듈을 사용하고, Java에서는 java.time 패키지를 사용한다.

## 관련 자료:
- Go 공식 문서: https://golang.org/pkg/time/
- time 패키지 예제: https://gobyexample.com/time
- Python datetime 모듈: https://docs.python.org/3/library/datetime.html
- Java java.time 패키지: https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html