---
title:                "Go: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# 왜
날짜를 문자열로 변환하는 것에 대해 관심을 가질 이유는 무엇일까요? 이 포스트에서는 읽는 이들이 이 작업을 통해 얻을 수 있는 이점에 대해 간단히 소개합니다.

## 해방 아웃
```Go
package main

import (
	"fmt"
	"time"
)

func main() {

	// 오늘 날짜
	today := time.Now()

	// 기본적인 문자열 변환
	fmt.Println(today.String())

	// 원하는 형식에 따른 문자열 변환
	fmt.Println(today.Format("2006년 01월 02일"))

	// 시간 포맷 지정하기
	fmt.Println(today.Format(time.RFC822))
}
```
위 코드를 실행하면 우리는 오늘의 날짜를 다양한 형식의 문자열로 변환하는 방법을 볼 수 있습니다. 결과는 다음과 같습니다.
```
2019-08-05 14:05:10.2350563 +0900 KST m=+0.003305901
2019년 08월 05일
05 Aug 19 14:05 KST
```
이처럼 날짜를 문자열로 변환하면 날짜 데이터를 보다 쉽게 다룰 수 있게 됩니다. 이는 웹 애플리케이션에서 날짜를 표시하거나, 데이터베이스에 저장하는 등 다양한 상황에서 유용하게 쓰일 수 있습니다.

## 깊이 파봄
날짜를 문자열로 변환하는 과정에서 어떤 로직이 일어나는지 더 깊이 파고들어봅시다. Go 언어에서는 내부적으로 `time` 패키지의 `String()` 메서드를 통해 날짜를 문자열로 변환합니다. 이 메서드는 기본적으로 특정 포맷으로 날짜를 출력하고, 만약 다른 형식이 필요하다면 `Format()` 메서드를 사용하면 됩니다. 또한 `time.RFC822` 같은 상수를 통해 공통적으로 쓰이는 시간 포맷을 이용할 수도 있습니다. 이렇듯 Go 언어는 편리하고 다양한 기능을 제공하여 날짜와 시간을 다룰 수 있게 만들어 줍니다.

## 또는 읽기
- [Go 언어 공식 문서 - "time" 패키지](https://golang.org/pkg/time/)
- [Go 언어를 써본 사람이라면 찾아보거나 읽어봐야 할 곳](https://www.alexedwards.net/blog/working-with-dates-and-times-in-go)