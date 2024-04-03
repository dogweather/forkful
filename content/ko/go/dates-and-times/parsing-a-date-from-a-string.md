---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:05:26.747265-07:00
description: "Go\uC5D0\uC11C \uBB38\uC790\uC5F4\uB85C\uBD80\uD130 \uB0A0\uC9DC\uB97C\
  \ \uD30C\uC2F1\uD558\uB294 \uAC83\uC740 \uD14D\uC2A4\uD2B8\uB85C \uD45C\uD604\uB41C\
  \ \uB0A0\uC9DC\uB97C \uB354 \uC0AC\uC6A9\uD558\uAE30 \uC88B\uC740 \uD615\uC2DD(\uC608\
  : `time.Time`)\uC73C\uB85C \uBCC0\uD658\uD558\uB294 \uC791\uC5C5\uC744 \uB9D0\uD569\
  \uB2C8\uB2E4. \uAC1C\uBC1C\uC790\uB4E4\uC774 \uC774 \uC791\uC5C5\uC744 \uC218\uD589\
  \uD558\uB294 \uAC83\uC740 \uD2B9\uD788 \uC0AC\uC6A9\uC790 \uC785\uB825, API, \uB610\
  \uB294 \uB0A0\uC9DC\uAC00 \uC885\uC885 \uBB38\uC790\uC5F4\uB85C \uD45C\uD604\uB418\
  \uB294 \uC800\uC7A5 \uC2DC\uC2A4\uD15C\uACFC \uAC19\uC740 \uACF3\uC5D0\uC11C\u2026"
lastmod: '2024-03-13T22:44:54.472502-06:00'
model: gpt-4-0125-preview
summary: "Go\uC5D0\uC11C \uBB38\uC790\uC5F4\uB85C\uBD80\uD130 \uB0A0\uC9DC\uB97C \uD30C\
  \uC2F1\uD558\uB294 \uAC83\uC740 \uD14D\uC2A4\uD2B8\uB85C \uD45C\uD604\uB41C \uB0A0\
  \uC9DC\uB97C \uB354 \uC0AC\uC6A9\uD558\uAE30 \uC88B\uC740 \uD615\uC2DD(\uC608: `time."
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC \uBD84\uC11D\uD558\uAE30"
weight: 30
---

## 방법:
Go는 `time` 패키지를 통해 날짜와 시간을 파싱하기 위한 강력한 지원을 제공합니다. 핵심은 Go의 참조 날짜 형식인 `Mon Jan 2 15:04:05 MST 2006`을 이해하는 것입니다. 이것은 Go에게 들어오는 문자열을 어떻게 해석할지 알려줍니다. 시작하기 위한 간단한 예제를 살펴보겠습니다:

```go
package main

import (
	"fmt"
	"time"
)

func main() {
	// 예제 날짜 문자열
	dateStr := "2023-04-12 14:45:00"
	
	// 입력 날짜 문자열의 형식/레이아웃을 정의
	// 이 레이아웃은 Go에게 연도, 다음으로 달,
	// 그 다음 날, 시간, 분, 마지막으로 초를 기대하도록 알려줍니다
	layout := "2006-01-02 15:04:05"
	
	// 레이아웃에 따라 날짜 문자열을 파싱
	parsedDate, err := time.Parse(layout, dateStr)
	if err != nil {
		fmt.Println("날짜 파싱 오류:", err)
		return
	}
	
	// 파싱된 날짜 출력
	fmt.Println("파싱된 날짜:", parsedDate)
}
```

이 코드를 실행하면 다음과 같은 결과를 얻습니다:

```
파싱된 날짜: 2023-04-12 14:45:00 +0000 UTC
```

참조 날짜의 값을 사용하여 입력 문자열의 형식을 지정하는 `layout` 문자열에 주목하세요. 입력 날짜의 형식과 일치하도록 `layout`을 조정하세요.

## 심층 학습
Go의 날짜와 시간 파싱 설계는 특정 참조 날짜(`Mon Jan 2 15:04:05 MST 2006`)를 사용하는 독특한 방식으로 이루어져 있습니다. 이 접근 방식은 년도에 대해 `YYYY`와 같은 더 전통적인 형식 지정자를 사용하는 대신, 가독성과 사용 용이성 차원에서 더 예제 기반의 형식을 채택했습니다.

이것은 다른 언어에 익숙한 개발자들에게 처음에는 평소와 다를 수 있지만, 잠시의 조정 기간 후에 많은 이들이 더 직관적으로 여기게 됩니다. Go의 `time` 패키지가 직접 지원하지 않는 보다 복잡한 날짜 조작이나 형식이 필요한 애플리케이션의 경우, `github.com/jinzhu/now`와 같은 서드파티 라이브러리가 추가 기능을 제공할 수 있습니다. 하지만, 대부분의 표준 애플리케이션에 대해, Go의 내장 기능은 견고하고, 효율적이며, Go의 단순함과 명확성 철학을 구현하고 있습니다.
