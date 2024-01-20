---
title:                "문자열에서 날짜 분석하기"
html_title:           "Gleam: 문자열에서 날짜 분석하기"
simple_title:         "문자열에서 날짜 분석하기"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가?

문자열에서 날짜를 파싱한다는 것은 문자열을 날짜 형태로 전환하는 것을 의미합니다. 이는 프로그래머가 사용자의 입력이나 외부 소스에서 날짜 정보를 받아서 내부적으로 작동하는 날짜 형식으로 형변환하는 경우에 사용됩니다.

## 사용법:

다음은 Go 언어로 문자열에서 날짜 형식을 파싱하는 방법입니다:
```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	const inputFormat = "2006-01-02"
	date, err := time.Parse(inputFormat, "2020-12-31")
	if err != nil {
		fmt.Println(err)
	}
	fmt.Println(date)
}
```

이 코드를 실행하면 다음과 같은 결과를 얻을 수 있습니다:

```Go
2020-12-31 00:00:00 +0000 UTC
```

## 깊게 알아보기:

날짜 파싱은 컴퓨터 사이언스의 초기 단계에서 문제 해결을 위해 개발되었다. 그 이후로 여러 언어에서 다양한 구현이 등장했으며 Go언어에서는 제공하는 time 패키지를 통해 구현할 수 있다.

하지만, 알트베이스나 다른 문자열 파싱 라이브러리를 사용하여 다른 방법으로 날짜 파싱을 수행할 수도 있다. 

특히 Go는 약간의 혼란을 야기할 수 있는 고유의 날짜/시간 형식을 가지고 있다. 이 형식은 "2006-01-02 15:04:05"에 기반하고 있으며, 이를 기억하면 문자열에서 날짜를 파싱하는데 도움이 됩니다.

## 관련 자료

다음은 날짜/시간 파싱에 관련된 추가 자료 링크입니다:

- [Go 공식 문서: 날짜와 시간](https://golang.org/pkg/time/)
- [Go 시간 포맷에 대한 흥미로운 글](https://yourbasic.org/golang/format-parse-string-time-date-example/)