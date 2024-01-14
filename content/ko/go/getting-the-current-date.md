---
title:                "Go: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why (왜):
현재 날짜를 가져오는 것에 대해 궁금한 이유는 다양합니다. 일반적으로, 사용자에게 현재 날짜를 보여주거나 특정 작업을 수행할 때 날짜를 기록할 필요가 있을 때가 있습니다.

## How To (어떻게):
Go 언어에서 현재 날짜를 가져오는 방법은 간단합니다. ```time``` 패키지의 ```Now()``` 함수를 사용하면 됩니다. 아래는 해당 함수를 사용하여 현재 날짜를 출력하는 간단한 예제 코드입니다.

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// 현재 날짜와 시간을 가져오기
	currentTime := time.Now()

	// 포맷 지정
	dateFormat := "2006년 01월 02일"

	// 현재 날짜를 지정한 포맷으로 출력
	fmt.Println("오늘의 날짜는:", currentTime.Format(dateFormat))
}
```

위 코드를 실행하면 다음과 같은 결과가 출력됩니다.

```
오늘의 날짜는: 2021년 04월 19일
```

## Deep Dive (더 깊게 알아보기):
Go 언어에서는 ```time``` 패키지를 사용하여 다양한 시간과 날짜 관련 작업을 수행할 수 있습니다. 해당 패키지에는 다양한 함수와 메소드가 포함되어 있으며, 이를 통해 날짜와 시간을 다양한 형식으로 출력하거나 비교하는 등의 작업을 할 수 있습니다. 좀 더 자세한 내용은 공식 Go 언어 문서를 참고하시기 바랍니다.

## See Also (관련 글):
- [Go 언어 공식 문서](https://golang.org/pkg/time/)
- [나만의 시계 앱 만들기 (Go + Vue.js)](https://medium.com/@sufuk/go-vuejs%EC%99%80-%EA%B8%B0%EB%B3%B8%EC%9D%84-%EC%9D%B4%EC%9A%A9%ED%95%9C-%EC%8B%9C%EA%B3%84-%EC%95%B1-%EB%A7%8C%EB%93%A4%EA%B8%B0-d350efc1d094)
- [Go언어로 날짜 시간 지역화하기](https://sub0709.tistory.com/13)