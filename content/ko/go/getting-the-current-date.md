---
title:                "현재 날짜 가져오기"
html_title:           "C: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

현재 날짜를 가져오는 것은 특정 시점의 날짜와 시간 정보를 프로그램에 제공하는 것입니다. 프로그래머들이 이를 수행하는 이유는 로깅, 타임스탬프 작성 및 실시간 기능을 제공하기 위해서입니다.

## 어떻게 하나요?

아래는 현재 날짜를 얻는 Go 프로그램의 예제입니다:

```Go
package main

import "fmt"
import "time"

func main() {
	currentTime := time.Now()
	fmt.Println("현재 시간:", currentTime)
}
```

이 코드를 실행하면 출력은 다음과 같습니다:

```Go
현재 시간: 2022-03-01 15:04:05.999999999 +0900 KST m=+0.000000001
```

이 코드는 `time` 패키지의 `Now` 함수를 사용하여 현재 시간을 얻습니다. 그런 다음 이것을 출력으로 반환하여 현재 시간을 출력합니다.

## 깊게 알아보기

과거에는 날짜와 시간 정보를 얻는 방법이 제한적이었습니다. 그러나 현재 Go의 `time` 패키지 덕분에 프로그래머들이 쉽게 현재 날짜와 시간 정보에 액세스할 수 있게 되었습니다.

현재 날짜를 얻는 대안은 `time` 패키지 외부에 있는 라이브러리를 사용하는 것이 있습니다. 그러나 이는 일반적으로 권장되지 않습니다. 왜냐하면 `time` 패키지는 그 자체로 충분하며, 외부 라이브러리는 종종 불필요한 복잡성을 추가하기 때문입니다.

우리가 사용한 `Now` 함수는 현재 날짜와 시간을 리턴하는데 있어 매우 중요한 역할을 합니다. 이것은 UTC (국제 표준시)를 사용하여 시간 정보를 제공하는데, 코드에 분명히 날짜와 시간 정보를 제공하는 방법입니다.

## 참고 자료

시간과 관련된 더 많은 예제와 토픽을 탐색하려면 다음 링크를 참조하십시오:

- Go 공식 문서: https://golang.org/pkg/time/
- Go에서의 날짜 및 시간 다루기: https://yourbasic.org/golang/format-parse-string-time-date-example/