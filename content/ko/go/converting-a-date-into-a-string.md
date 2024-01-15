---
title:                "날짜를 문자열로 변환하기"
html_title:           "Go: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜?

*왜* 어떤 사람이 날짜를 문자열로 변환하는 작업에 참여해야 할까요?

일반적인 회사 업무나 앱 개발에서, 날짜 형식으로 데이터를 저장하고 전달하는 일이 매우 흔합니다. 이때 변환 함수를 사용하면, 날짜 데이터를 다른 형식으로 쉽게 전환할 수 있습니다.

## 어떻게 해야 할까?

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    // Go에서 현재 날짜 가져오기
    todaysDate := time.Now()

    // 날짜를 문자열로 변환하기
    dateString := todaysDate.Format("2006년 01월 02일 (월) 15시 04분")
    fmt.Println(dateString) // 출력: 2020년 11월 06일 (금) 11시 41분 
}
```

위의 코드를 실행하면 현재 날짜가 해당 지역의 시간대를 기준으로 "2006년 01월 02일 (월) 15시 04분" 형식의 문자열로 출력됩니다. 이렇게 변환된 날짜 데이터는 다른 형식으로 전환하거나 출력을 원하는 대로 필요한 곳에 활용할 수 있습니다.

## 더 깊게 들어가보기

위의 예시에서 사용한 `time` 패키지의 `Now()` 함수와 `Format()` 메서드는 Go에서 날짜와 시간 데이터를 다루는데 매우 중요한 역할을 합니다. 이들 함수를 조합해 여러 가지 형식의 날짜 데이터를 생성하거나, 원하는 형식으로 변환할 수 있습니다.

그리고 `Format()` 메서드에 사용되는 문자열은 미리 정의된 형식(예: "2006년 01월 02일")을 포맷 템플릿으로 사용하여, 해당 형식에 맞는 문자열로 날짜 데이터를 변환할 수 있도록 합니다. 따라서 우리가 직접 원하는 형식의 날짜 문자열을 만들 수도 있습니다.

## 함께 보기

- [Go Time 패키지 문서](https://golang.org/pkg/time/)
- [Go 날짜 형식 샘플 코드](https://play.golang.org/p/FCEihRBlVrh)

# 관련 링크

- [Go 공식 문서](https://golang.org/doc/)
- [Go 언어 특징과 사용 예시](https://www.edwith.org/golang/)
- [Go 언어로 쇼핑몰 만들기](https://github.com/shawon100/go-shopping-mall)