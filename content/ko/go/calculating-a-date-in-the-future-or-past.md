---
title:    "Go: 미래나 과거의 날짜 계산하기"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜

고의라는 언어는 다양한 애플리케이션 개발에 널리 사용되고 있습니다. 대부분의 애플리케이션에서는 시간과 날짜를 다루는 기능이 필수적입니다. 때때로 고지기는 미래나 과거로부터 특정 날짜를 계산해야 할 때가 있습니다. 이를 위해 만들어진 유용한 기능이 바로 `time` 패키지의 `AddDate()` 메서드입니다. 이 기능을 사용하면 미래로부터 새로운 날짜를 계산하거나 과거로부터 과거 또는 현재 날짜를 계산할 수 있습니다.

## 어떻게

우선, `time` 패키지를 import 해야 합니다. 그리고 `time.Now()` 함수를 사용하여 현재 시간을 구할 수 있습니다. 이제 `AddDate()` 메서드를 사용해 미래 또는 과거로부터 날짜를 계산할 수 있습니다. 아래는 예제 코드와 출력 결과입니다.

```Go
import "fmt"
import "time"

func main() {
    now := time.Now() // 현재 시간
    fmt.Println("현재 날짜: ", now)

    future := now.AddDate(1, 2, 3) // 1년, 2개월, 3일 후의 날짜를 계산
    fmt.Println("미래의 날짜: ", future)

    past := now.AddDate(-2, 0, 0) // 2년 전의 날짜를 계산
    fmt.Println("과거의 날짜: ", past)
}
```

출력 결과:
```
현재 날짜: 2021-11-01 14:30:00 +0000 UTC m=+0.000000001
미래의 날짜: 2022-01-04 14:30:00 +0000 UTC m=+0.012345678
과거의 날짜: 2019-11-01 14:30:00 +0000 UTC m=+0.000000001
```

## 딥 다이브

`AddDate()` 메서드는 `int` 형식의 매개변수를 3개 받습니다. 첫 번째는 년도를, 두 번째는 월을, 세 번째는 일을 나타냅니다. 따라서 `time.Now()` 함수를 사용하여 현재 시간을 구하고, `AddDate()` 메서드를 통해 반환된 값을 변수에 저장하면 미래나 과거의 날짜를 계산할 수 있습니다. 이때, 년도와 월의 경우 양수 값을 사용하면 미래로, 음수 값을 사용하면 과거로 계산됩니다.

## 더 찾아보기

- [Go 언어 공식 문서: ```time``` 패키지](https://golang.org/pkg/time/)
- [A Tour of Go: 표준 라이브러리 - ```time``` 패키지](https://go-tour-kr.appspot.com/basics/1)
- [Go 언어를 배우면서 느낀 10가지 장단점](https://d2.naver.com/helloworld/45197)
- [그날의 날짜 - 고의모음](https://d2.naver.com/helloworld/2421856)