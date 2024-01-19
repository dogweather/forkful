---
title:                "두 날짜 비교하기"
html_title:           "C#: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 무엇이고 왜 필요한가?
두 날짜를 비교한다는 것은 프로그램에서 두 날짜 간의 차이를 계산하는 것을 의미합니다. 이것은 프로그래머들이 우리가 일일이 계산할 필요 없도록, 시간을 관리하거나 일정을 추적하게 돕기 위해 일반적으로 사용됩니다. 

## 어떻게 하나
Go 언어를 사용하여 두 날짜를 비교하는 간단한 코드 예제를 살펴보겠습니다.

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    date1 := time.Date(2021, 12, 31, 0, 0, 0, 0, time.UTC)
    date2 := time.Date(2022, 01, 01, 0, 0, 0, 0, time.UTC)

    if date1.Before(date2) {
        fmt.Println(date1.Format("2006-01-02"), "is before", date2.Format("2006-01-02"))
    } else if date1.After(date2) {
        fmt.Println(date1.Format("2006-01-02"), "is after", date2.Format("2006-01-02"))
    } else {
        fmt.Println(date1.Format("2006-01-02"), "is the same day as", date2.Format("2006-01-02"))
    }
}
```
당신이 이 코드를 실행하면, 출력은 `2021-12-31 is before 2022-01-01`이 될 것입니다.

## 깊게 파보기
두 날짜를 비교하는 것은 프로그래밍의 초기 단계부터 존재해왔습니다. Go에서는 time 패키지를 사용하여 이 작업을 수행한다. 이 패키지는 Go1 버전 부터 포함되었으며, 표준 라이브러리의 일부입니다. Before, After, and Equal 같은 메소드를 이용해서 우리는 쉽게 시간을 비교할 수 있습니다.

대안으로, Unix 타임스탬프를 통해 날짜를 비교하는 방법도 있습니다. 이 경우 두 날짜의 차이를 초 단위로 계산합니다. 하지만, 이 방법은 직관적이지 않고 오류를 발생시킬 수 있으므로, 일반적으로는 time 패키지를 사용하는 것이 권장됩니다.

## 참고 자료
- [Go Docs: Time Package](https://pkg.go.dev/time): Go 언어의 time 패키지에 대한 공식 문서입니다. 이 패키지의 기능과 메소드에 대해 자세히 알아봅시다.
- [Go Playground: Date Comparison](https://play.golang.org/p/PYpXil6l7tT): Go Playground에서 실행 가능한 날짜 비교 코드 예제입니다. 직접 실행해보고 변형해보세요.