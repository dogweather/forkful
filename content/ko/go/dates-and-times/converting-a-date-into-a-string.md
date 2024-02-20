---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:54:36.832173-07:00
description: "Go\uC5D0\uC11C \uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\
  \uD558\uB294 \uAC83\uC740 `time.Time` \uAC1D\uCCB4\uB97C \uC77D\uC744 \uC218 \uC788\
  \uB294 \uBB38\uC790\uC5F4 \uD615\uC2DD\uC73C\uB85C \uBCC0\uD658\uD558\uB294 \uACFC\
  \uC815\uC744 \uB9D0\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740\
  \ \uC0AC\uC6A9\uC790 \uCE5C\uD654\uC801\uC778 \uBC29\uC2DD\uC73C\uB85C \uB0A0\uC9DC\
  \uB97C \uD45C\uC2DC\uD558\uAC70\uB098, \uC77C\uAD00\uB41C \uD615\uC2DD\uC73C\uB85C\
  \ \uB0A0\uC9DC\uB97C \uC800\uC7A5\uD558\uACE0 \uC804\uC1A1\uD558\uAE30 \uC704\uD574\
  \ \uC774 \uC791\uC5C5\uC744 \uC885\uC885 \uC218\uD589\uD569\uB2C8\uB2E4."
lastmod: 2024-02-19 22:05:13.419159
model: gpt-4-0125-preview
summary: "Go\uC5D0\uC11C \uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\uD558\
  \uB294 \uAC83\uC740 `time.Time` \uAC1D\uCCB4\uB97C \uC77D\uC744 \uC218 \uC788\uB294\
  \ \uBB38\uC790\uC5F4 \uD615\uC2DD\uC73C\uB85C \uBCC0\uD658\uD558\uB294 \uACFC\uC815\
  \uC744 \uB9D0\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC0AC\
  \uC6A9\uC790 \uCE5C\uD654\uC801\uC778 \uBC29\uC2DD\uC73C\uB85C \uB0A0\uC9DC\uB97C\
  \ \uD45C\uC2DC\uD558\uAC70\uB098, \uC77C\uAD00\uB41C \uD615\uC2DD\uC73C\uB85C \uB0A0\
  \uC9DC\uB97C \uC800\uC7A5\uD558\uACE0 \uC804\uC1A1\uD558\uAE30 \uC704\uD574 \uC774\
  \ \uC791\uC5C5\uC744 \uC885\uC885 \uC218\uD589\uD569\uB2C8\uB2E4."
title: "\uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇과 왜?

Go에서 날짜를 문자열로 변환하는 것은 `time.Time` 객체를 읽을 수 있는 문자열 형식으로 변환하는 과정을 말합니다. 프로그래머들은 사용자 친화적인 방식으로 날짜를 표시하거나, 일관된 형식으로 날짜를 저장하고 전송하기 위해 이 작업을 종종 수행합니다.

## 방법:

Go에서는 `time` 패키지가 날짜와 시간을 다루는 기능을 제공하며, 이에는 `time.Time` 객체를 문자열로 포맷팅하는 기능도 포함됩니다. 이 목적을 위해 `time.Time` 타입의 `Format` 메서드를 사용하며, 여기서는 참조 시간인 "Mon Jan 2 15:04:05 MST 2006"에 따라 레이아웃 문자열을 지정합니다.

### 예시:

```go
package main

import (
	"fmt"
	"time"
)

func main() {
	currentTime := time.Now() // 현재 날짜와 시간을 가져옵니다
	fmt.Println("Current Time:", currentTime)

	// 현재 시간을 dd-mm-yyyy 형식으로 포맷팅합니다
	formattedDate := currentTime.Format("02-01-2006")
	fmt.Println("Formatted Date:", formattedDate)

	// 현재 시간을 보다 자세히 포맷팅합니다
	detailedFormat := currentTime.Format("Mon, 02 Jan 2006 15:04:05 MST")
	fmt.Println("Detailed Formatted Date:", detailedFormat)
}
```

#### 샘플 출력:

```
Current Time: 2023-04-12 11:45:20.312457 +0000 UTC
Formatted Date: 12-04-2023
Detailed Formatted Date: Wed, 12 Apr 2023 11:45:20 UTC
```

프로그램을 실행할 때 현재 날짜와 시간에 따라 출력 결과가 달라집니다.

## 심층 분석:

Go의 맥락에서 날짜와 시간 조작은 포맷팅을 포함하여 주로 `time` 패키지에 의해 처리됩니다. Go에서 특정 레이아웃 문자열을 사용하여 `Format` 메서드로 지정한 날짜 포맷팅 접근 방식은 4자리 연도에 대해 `%Y`와 같은 간단한 형식 지정자를 사용할 수 있는 많은 다른 프로그래밍 언어와 비교하여 독특합니다. Go 방식에서는 포맷팅이나 파싱 날짜에 대한 패턴으로 작용하는 특정 참조 시간인 Mon Jan 2 15:04:05 MST 2006을 개발자가 기억해야 합니다.

이 방법은 처음에는 strftime과 같은 포맷팅 기능에 익숙한 개발자들에게 직관적이지 않게 보일 수 있지만, 명확성을 위해 설계되었고 지역에 따라 다른 형식의 혼란을 피하기 위한 것입니다. 익숙해지면 많은 이들이 이 접근법이 오류를 줄이고 코드 가독성을 향상시킨다고 느낍니다.

또한, Go의 표준 라이브러리 접근 방식은 대부분의 일반적인 사용 사례에 대해 타사 라이브러리가 필요 없음을 의미합니다. 이는 의존성 관리를 단순화하고 다양한 프로젝트 간에 일관된 동작을 보장합니다. 하지만, 더 복잡한 시간대 변환 또는 반복되는 날짜 계산을 작업할 때 개발자는 휴일 계산을 위한 `github.com/rickar/cal`이나 표준 `time` 패키지가 제공하는 것을 넘어서는 더 미묘한 시간 조작을 위한 `github.com/golang/time`과 같은 추가 패키지를 찾아볼 필요가 있을 수 있습니다.
