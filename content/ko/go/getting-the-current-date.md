---
title:    "Go: 현재 날짜 받기"
keywords: ["Go"]
---

{{< edit_this_page >}}

# 왜 날짜를 구하는 것이 중요한가요?

날짜는 우리 일상에서 매우 중요합니다. 특정 작업을 수행하거나 계획을 세울 때 날짜를 알아야 합니다. 따라서 프로그램에서도 현재 날짜를 구하는 기능은 매우 유용합니다. 사용자에게 정확한 날짜를 제공하여 프로그램을 더욱 신뢰성 있게 만들 수 있습니다.

## 날짜를 구하는 방법

Go에서는 "time" 패키지를 사용하여 현재 날짜를 구할 수 있습니다. 다음은 현재 날짜를 구하는 예제 코드입니다.

```Go
package main

import (
  "fmt"
  "time"
)

func main() {
  // 현재 날짜와 시간을 구합니다.
  now := time.Now()

  // 현재 날짜 출력 형식을 설정합니다.
  layout := "2006년 1월 2일"

  // 현재 날짜를 설정한 형식으로 출력합니다.
  fmt.Println(now.Format(layout))
}
```

이 코드를 실행하면 "2020년 8월 17일"과 같은 형식으로 현재 날짜가 출력됩니다.

## 깊이 들어가기

Go에서 날짜는 "time.Time" 타입으로 표현됩니다. 이 타입에는 년, 월, 일, 시간, 분, 초, 요일 등의 정보가 포함되어 있습니다. 따라서 코드에서 원하는 형식으로 날짜를 출력할 수 있도록 Format 함수를 사용해야 합니다. 또한 날짜를 연산하려면 "time.Duration" 타입을 사용해야 합니다.

# 참고 자료

- [Go 공식 문서 - Time 패키지](https://golang.org/pkg/time/)
- [Go 언어 러닝 가이드 - 시간 및 시간대](https://learnku.com/docs/the-way-to-go/time-and-timezones/3619)
- [Go 언어 코딩 스타일 - 시간 사용하기](https://draveness.me/golang/docs/part1-prerequisite/ch05-keyword/golang-time-date/)