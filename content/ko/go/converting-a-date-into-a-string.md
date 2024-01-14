---
title:    "Go: 날짜를 문자열로 변환하기"
keywords: ["Go"]
---

{{< edit_this_page >}}

# 왜
날짜를 문자열로 변환하는 과정에 참여하는 이유를 간단히 설명합니다.

## 어떻게

날짜를 문자열로 변환하는 방법을 코드 예제와 함께 "```Go ... ```" 코드 블록 내에 표시합니다. 예제 출력물도 포함됩니다.

```Go
// 현재 날짜 및 시간 가져오기
now := time.Now()

// 날짜를 문자열로 변환하기
dateString := now.Format("2006년 1월 2일")

// 출력: 2019년 7월 4일
fmt.Println(dateString)
```

## 깊게 들어가기

날짜를 문자열로 변환하는 보다 깊은 내용을 다룹니다. 이 과정이 어떻게 이루어지고, Go 언어에서는 어떤 함수를 사용하는지 등의 정보를 제공합니다.

문자열로 변환하는 방법뿐만 아니라, 시간 형식을 지정하고 다른 출력 포맷을 사용하는 방법 등도 다룹니다.

## 참고 자료

[Go 언어 공식 문서 - 시간 포매팅](https://golang.org/pkg/time/#Time.Format)

[Medium 블로그 - Let's Go: 시간](https://medium.com/@hengilas/lets-go-%EC%8B%9C%EA%B0%84-7abb2112d4d)

[Wikibooks - Go 프로그래밍 - 시간](https://ko.wikibooks.org/wiki/Go_%ED%94%84%EB%A1%9C%EA%B7%B8%EB%9E%A8/%ED%8A%B9%EC%A7%95/%EC%8B%9C%EA%B0%84)