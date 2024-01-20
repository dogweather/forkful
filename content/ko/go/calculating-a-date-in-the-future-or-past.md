---
title:                "미래 또는 과거의 날짜 계산하기"
html_title:           "Go: 미래 또는 과거의 날짜 계산하기"
simple_title:         "미래 또는 과거의 날짜 계산하기"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

날짜 계산은 특정 날짜 기준으로 미래나 과거의 날짜를 찾는 것을 말합니다. 예약 시스템, 더미 데이터 생성, 날짜 기반 쿼리 등 프로그래머가 여러 이유로 이를 실행할 수 있습니다.

## 어떻게:

먼저, `time` 패키지를 import 한 다음, `Now` 함수를 사용하여 현재 시간을 가져옵니다. 그런 다음, `AddDate` 메서드를 사용하여 특정 연도, 월, 날짜를 더하거나 빼면 됩니다.

```Go
package main
import (
    "fmt"
    "time"
)

func main() {
    t := time.Now()
    fmt.Println("Current Date:", t.Format("2006-01-02"))

    futureDate := t.AddDate(1, 2, 3)
    fmt.Println("Future Date: ", futureDate.Format("2006-01-02"))

    pastDate := t.AddDate(-1, -2, -3)
    fmt.Println("Past Date: ", pastDate.Format("2006-01-02"))
}
```

이 코드 블록을 실행하면 다음과 같은 출력결과가 나옵니다:

```Go
Current Date: 2021-08-31
Future Date:  2022-11-03
Past Date:    2020-06-28
```

## 딥 다이브

초기에 날짜 계산은 어렵고 복잡한 작업이었습니다. 통상적으로 용어를 계산하고 천문학적 이벤트를 예측하는 데 필요한 천문학자가 직접 실행했습니다. 하지만 Go 외의 여러 프로그래밍 언어와 도구들이 현대 컴퓨터에서는 이 작업을 간단하게 만들어 가게 되었습니다.

대안으로는 `time` 패키지 외에 `date`, `now` 등의 날짜 및 시간 처리 라이브러리도 있습니다.

날짜 계산의 구현 세부 사항이 다소 복잡할 수 있는 이유는 서로 다른 시간대, 윤년, 윤초, 그리고 기타 역사적인 조정에 대한 지속적인 계산 뿐만 아니라 다양한 표준이 존재하기 때문입니다.

## 참고 자료

- [Go 문서](https://golang.org/pkg/time/)
- [`time` 패키지에 대한 Go 블로그](https://blog.golang.org/toward-go2)