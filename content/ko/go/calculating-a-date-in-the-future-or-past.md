---
title:                "'미래나 과거의 날짜를 계산하는 방법'"
html_title:           "Go: '미래나 과거의 날짜를 계산하는 방법'"
simple_title:         "'미래나 과거의 날짜를 계산하는 방법'"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
미래나 과거의 날짜를 계산하는 것은 프로그래머들이 자주 하는 작업입니다. 이는 프로그램에서 시간과 날짜를 다루기 위해 필요한 기능 중 하나입니다. 예를 들어, 다음달에 어떤 이벤트가 발생하는지 알고 싶을 때 프로그램에서 이전 월을 계산할 필요가 있습니다.

## 방법:
Go 언어에서 날짜를 계산하는 방법은 다양합니다. 가장 쉬운 방법은 현재 날짜를 기준으로 더하거나 빼는 것입니다. 예를 들어, 다음주에 어떤 작업을 해야한다면 현재 날짜에 7일을 더하면 됩니다. 아래의 예제 코드를 참고하세요.

```Go
package main
import "fmt"
import "time"

func main() {
    t := time.Now() // 현재 날짜와 시간 가져오기
    future := t.AddDate(0, 0, 7) // 현재 날짜에서 7일 더해주기
    fmt.Println("다음주가 무슨 작업이 있나요?", future) // 출력 결과: 다음 주에는 <날짜>에 무슨 작업이 있나요?
}
```

```Go
package main
import "fmt"
import "time"

func main() {
    t := time.Now() // 현재 날짜와 시간 가져오기
    past := t.AddDate(0, 0, -7) // 현재 날짜에서 7일 빼주기
    fmt.Println("지난주에는 무슨 작업이 있었나요?", past) // 출력 결과: 지난 주에는 <날짜>에 무슨 작업이 있었나요?
}
```

## 깊이 파보기:
과거와 미래의 날짜를 계산하는 기능은 프로그래밍에서 흔히 사용되지만, 과거에는 많은 맥락과 계산 방법이 달랐습니다. 예를 들어, 로마 제국에서는 매달 1일로부터 날짜를 세는 방식을 사용했고 프랑스 혁명 이후에는 매달 0일로부터 날짜를 세는 방식을 사용했습니다.

날짜 계산을 위해 Go 언어 이외에도 많은 프로그래밍 언어들이 지원하고 있습니다. 주로 사용되는 다른 언어로는 JavaScript, Python, Java 등이 있습니다.

Go 언어에서는 날짜를 계산할 때 time 패키지를 사용합니다. 이 패키지에는 다양한 기능들이 있으며 날짜 계산 외에도 시간 변환, 날짜 포맷 지정 등 다양한 기능을 제공합니다.

## 관련 자료:
- Go 언어 공식 문서: https://golang.org/pkg/time/
- 날짜와 시간 다루기 예제: https://gobyexample.com/time