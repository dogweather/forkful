---
title:                "날짜를 문자열로 변환하기"
html_title:           "Arduino: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
날짜를 문자열로 변환하는 것은 고유한 형태의 날짜를 읽을 수 있는 형태로 변경하는 과정입니다. 프로그래머들이 이를 수행하는 이유는 날짜를 사용자가 이해할 수 있는 방식으로 표시하거나, 특정 포맷에 맞추기 위함입니다.

## 어떻게 하는가:
Go 언어를 사용하면 `time` 패키지의 `Format` 메소드를 이용해 쉽게 날짜를 문자열로 변환할 수 있습니다. 예를 들어, 다음과 같이 코드를 작성할 수 있습니다:

```Go
package main
import "fmt"
import "time"

func main() {
    t := time.Now()
    fmt.Println(t.Format("2006-01-02 15:04:05"))
}
```
해당 코드를 실행하면, 현재 시간을 `YYYY-MM-DD hh:mm:ss` 형식으로 출력하는 것을 확인할 수 있습니다.

## 깊이 들어가보기:
날짜를 문자열로 변환하는 것은 컴퓨터 프로그래밍의 초기부터 존재해 왔으며, 사용자에게 정보를 제공하는 데 중요한 역할을 합니다. Go에서는 이를 구현하기 위해 'reference time'이라는 솔루션을 사용합니다. 즉, Go의 표준 시간인 `2006-01-02 15:04:05`를 사용하여 날짜와 시간을 원하는 모든 포맷으로 변환할 수 있습니다.

물론, 대안 방법도 있습니다. 만약 더 복잡한 날짜 포맷 변환이 필요한 경우, Go에서 제공하는 `Sprintf `함수를 사용하여 원하는 포맷을 정의하는 것도 가능합니다.

## 참조:
더 많은 정보와 관련 코드 예제를 찾으려면, 아래 리소스를 참조하시기 바랍니다:
- Go로 날짜를 문자열로 변환하기: https://golang.org/pkg/time/#Time.Format
- Go 날짜 및 시간 포맷: http://golangcookbook.com/chapters/dates-and-times/formatting/