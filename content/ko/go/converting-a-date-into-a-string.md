---
title:                "Go: 날짜를 문자열로 변환하기"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜

일반적으로, 프로그래밍에서 날짜를 문자열로 변환하는 것은 출력 또는 데이터베이스에 저장하기 위해 필요합니다. 이 프로세스는 실제로 표준화되어 있지 않기 때문에 *문자열로 날짜 변환*에 대해 배워보는 것이 중요합니다.

## 하는 법

### 예제 1

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    // 현재 시간 가져오기
    now := time.Now()

    // 원하는 시간 형식 정의 및 문자열로 변환
    format := "2006-01-02"
    dateString := now.Format(format)

    // 결과 출력
    fmt.Println(dateString)
}
```

### 예제 1 출력

```
2021-03-17
```

### 예제 2

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    // 현재 시간 가져오기
    now := time.Now()

    // 원하는 시간 형식 정의 및 문자열로 변환
    format := "Monday, January 2, 2006"
    dateString := now.Format(format)

    // 결과 출력
    fmt.Println(dateString)
}
```

### 예제 2 출력

```
Wednesday, March 17, 2021
```

## 깊이 파고들기

`time` 패키지의 `Format()` 함수를 사용하여 날짜를 원하는 형식의 문자열로 변환할 수 있습니다. 이 함수는 `string` 타입을 반환하며, 첫 번째 매개변수는 출력할 형식을 나타내는 문자열입니다. 날짜 형식을 나타내는 소문자 `y`는 년도를, `m`은 월을, `d`는 일을 나타내며 소문자를 여러 번 사용하면 형식을 변경할 수 있습니다. 대문자 `M`은 월을 나타내며 여러 번 사용하면 영어로 된 월 이름을 출력할 수 있습니다. 여러분이 원하는 형식을 정확하게 알고 있으면, 이 함수는 날짜를 일치하는 문자열로 변환해줍니다.

## 관련 링크들

* [Go 언어 공식 문서 - `time` 패키지](https://golang.org/pkg/time/)
* [Go 언어 블로그 - `time` 패키지 소개](https://blog.golang.org/working-with-time)
* [Go Web Programming 블로그 - 날짜와 시간 포맷팅](https://gowebexamples.com/datetime-formatting/)