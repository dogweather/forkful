---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:57:55.251390-07:00
description: "\uC5B4\uB5BB\uAC8C \uD558\uB294\uAC00: Go\uC5D0\uC11C\uB294 `time` \uD328\
  \uD0A4\uC9C0\uAC00 \uB0A0\uC9DC \uBC0F \uC2DC\uAC04\uC744 \uB2E4\uB8E8\uB294 \uAC8C\
  \uC774\uD2B8\uC6E8\uC774\uC785\uB2C8\uB2E4. `time.Now()` \uD568\uC218\uB294 \uD604\
  \uC7AC \uB0A0\uC9DC\uC640 \uC2DC\uAC04\uC744 \uC81C\uACF5\uD558\uBA70, \uB2E4\uB978\
  \ \uD568\uC218 \uBC0F \uBA54\uC18C\uB4DC\uB294 \uC774 \uB370\uC774\uD130\uB97C \uD615\
  \uC2DD\uD654\uD558\uAC70\uB098 \uC870\uC791\uD558\uB294 \uB370 \uC0AC\uC6A9\uD560\
  \ \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uB2E4\uC74C\uC740 \uD604\uC7AC \uB0A0\uC9DC\uC640\
  \ \uADF8 \uB2E4\uC591\uD55C \uD45C\uD604\uC744 \uC5BB\uB294\u2026"
lastmod: '2024-03-13T22:44:54.474091-06:00'
model: gpt-4-0125-preview
summary: "Go\uC5D0\uC11C\uB294 `time` \uD328\uD0A4\uC9C0\uAC00 \uB0A0\uC9DC \uBC0F\
  \ \uC2DC\uAC04\uC744 \uB2E4\uB8E8\uB294 \uAC8C\uC774\uD2B8\uC6E8\uC774\uC785\uB2C8\
  \uB2E4."
title: "\uD604\uC7AC \uB0A0\uC9DC \uC5BB\uAE30"
weight: 29
---

## 어떻게 하는가:
Go에서는 `time` 패키지가 날짜 및 시간을 다루는 게이트웨이입니다. `time.Now()` 함수는 현재 날짜와 시간을 제공하며, 다른 함수 및 메소드는 이 데이터를 형식화하거나 조작하는 데 사용할 수 있습니다. 다음은 현재 날짜와 그 다양한 표현을 얻는 방법입니다:

```go
package main

import (
	"fmt"
	"time"
)

func main() {
	currentTime := time.Now() // 현재 날짜와 시간을 가져옴
	fmt.Println("현재 시간:", currentTime)

	// YYYY-MM-DD 형식으로 날짜를 얻기
	fmt.Println("현재 날짜:", currentTime.Format("2006-01-02"))

	// 날짜의 개별 구성 요소를 얻기
	year, month, day := currentTime.Date()
	fmt.Printf("년: %d, 월: %s, 일: %d\n", year, month, day)

	// 요일을 얻기
	fmt.Println("요일:", currentTime.Weekday())
}
```

샘플 출력은 다음과 같을 수 있습니다:

```
현재 시간: 2023-04-18 15:04:05.123456 +0000 UTC
현재 날짜: 2023-04-18
년: 2023, 월: April, 일: 18
요일: Tuesday
```

`Format`은 특정 날짜(2006-01-02)를 레이아웃 문자열로 사용한다는 것에 유의해야 합니다. 이는 Go에서 선택한 참조 날짜로, 날짜 형식화를 위한 기억법 패턴으로 사용됩니다.

## 심층 분석
Go에서 날짜와 시간 조작을 위해 `time` 패키지를 사용하기로 한 결정은 강력하고 직관적인 표준 라이브러리에 대한 언어의 헌신을 반영합니다. 일부 언어가 날짜 조작을 위해 여러 경쟁 라이브러리나 방법론을 가질 수 있는 반면, Go는 단일하고 잘 문서화된 표준을 우선시합니다.

Go의 시간 형식화에서 참조 날짜(`Mon Jan 2 15:04:05 MST 2006`)의 독특한 선택은 처음에는 혼란스러울 수 있지만, 실제로 사용성 측면에서의 걸작입니다. 프로그래머들이 다른 언어들이 사용할 수 있는 기호나 토큰을 암기하는 대신 예제 기반 접근법을 사용해 날짜와 시간 형식을 표현할 수 있게 합니다.

그러나 `time` 패키지는 대부분의 필요에 대해 광범위한 기능을 제공하긴 하지만, 시간대와 일광 절약 시간(DST) 변경에 대해 다루는 것은 때때로 새로운 Go 프로그래머들을 혼란스럽게 할 수 있습니다. Go가 위치별 시간을 어떻게 처리하는지 이해하는 것은 시간 조작에서 흔히 발생하는 문제를 피하는 데 중요합니다.

보다 복잡한 스케줄링이나 시간 조작이 필요한 경우, `github.com/robfig/cron`과 같은 Go용 서드파티 라이브러리가 표준 `time` 패키지보다 더 전문화된 기능을 제공할 수 있습니다. 그러나 현재 날짜와 시간을 얻고 처리하는 대부분의 응용 프로그램의 경우, `time` 패키지는 Go에서 견고하고 관용적인 출발점을 제공합니다.
