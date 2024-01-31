---
title:                "문자열에서 날짜 파싱하기"
date:                  2024-01-20T15:36:24.857842-07:00
html_title:           "Arduino: 문자열에서 날짜 파싱하기"
simple_title:         "문자열에서 날짜 파싱하기"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇 & 왜?)
문자열에서 날짜 파싱이란 문자열 형식의 날짜를 컴퓨터가 이해할 수 있는 날짜 객체로 변환하는 과정입니다. 프로그래머가 이 작업을 수행하는 이유는 사용자로부터 날짜를 입력받아 이를 처리, 저장, 비교하기 위해서입니다.

## How to: (어떻게 하나:)
```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// 예제 문자열 날짜
	dateStr := "2023-04-12T15:04:05Z"

	// time 패키지의 파싱 포맷
	layout := time.RFC3339

	// 문자열을 time.Time 형으로 파싱
	parsedDate, err := time.Parse(layout, dateStr)
	if err != nil {
		fmt.Println("날짜 파싱 에러:", err)
		return
	}

	// 파싱된 날짜 출력
	fmt.Println("파싱된 날짜:", parsedDate)
}
```
샘플 출력:
```
파싱된 날짜: 2023-04-12 15:04:05 +0000 UTC
```

## Deep Dive (심층 탐구)
계산기와 컴퓨터 초기 단계에서 날짜 파싱은 주로 간단한 형식으로 이루어졌습니다. Go 언어에는 `time` 패키지가 내장되어 있어 표준화된 RFC3339, RFC1123 같은 많은 날짜 형식을 지원합니다. `Parse` 함수는 특정 포맷의 문자열을 `time.Time` 타입으로 변환합니다. 

대안으로, 복잡한 날짜 처리를 위해 `github.com/araddon/dateparse` 라이브러리 같은 서드 파티 라이브러리를 사용할 수도 있습니다. 이러한 라이브러리들은 다양한 형식과 로케일을 지원하며 사용법이 간단합니다.

Go의 날짜 파싱은 내부적으로 레이아웃 문자열을 사용하여 주어진 날짜 형식을 해석합니다. 이 때 사용되는 'layout'은 2006년 1월 2일 오후 3시 4분 5초 UTC의 예시 날짜를 기준으로 사용하는 특이한 설정 방식이라고 할 수 있습니다.

## See Also (관련 자료)
- Go by Example: Time Formatting / Parsing: https://gobyexample.com/time-formatting-parsing
- Go `time` Package Documentation: https://golang.org/pkg/time/
- The `araddon/dateparse` Library: https://github.com/araddon/dateparse
- Go Blog on Time: https://blog.golang.org/time
