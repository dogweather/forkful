---
title:                "부분 문자열 추출하기"
html_title:           "Go: 부분 문자열 추출하기"
simple_title:         "부분 문자열 추출하기"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜

문자열을 추출하는 작업은 프로그래밍에서 빈번하게 발생하는 일 중 하나입니다. 예를 들어, 사용자가 입력한 이메일 주소에서 도메인 이름만 추출하거나, 이미지 URL에서 파일 확장자를 추출하는 등 다양한 상황에서 유용하게 사용될 수 있습니다.

## 추출하는 방법

```Go
// 문자열에서 도메인 이름 추출하기
func extractDomain(email string) string {
    atIndex := strings.Index(email, "@") // "@" 기호를 찾아 해당 위치를 변수에 저장합니다.
    domain := email[atIndex+1:] // "@" 기호 다음부터 도메인 이름이므로, 해당 부분만 추출합니다.
    return domain 
}
```

위 예시 코드에서는 `strings` 패키지의 `Index()` 함수를 사용하여 주어진 문자열에서 "@" 기호를 찾고, `[]` 기호를 이용하여 문자열을 슬라이싱하여 도메인 이름을 추출합니다. 이렇게 추출한 도메인 이름은 `extractDomain()` 함수의 반환값으로 사용됩니다.

추출할 문자열의 위치나 길이를 제어하는데 매우 유용한 `[]` 기호를 이용하여 원하는 부분만 추출할 수 있습니다. 예를 들어, 파일 이름에서 확장자만 추출하는 등 다양한 방식으로 활용할 수 있습니다.

```
## 딥 다이브

위에서 예시로 들었던 `strings.Index()` 함수에 대해 자세히 알아보겠습니다. 이 함수는 문자열에서 주어진 문자나 부분 문자열의 첫 번째 위치를 반환합니다. 만약 주어진 문자나 부분 문자열이 없다면, `-1` 값을 반환합니다.

또한 `[]` 기호를 이용하여 문자열을 슬라이싱하는 방법도 중요합니다. 이를 이용하여 하나의 문자를 추출하거나, 범위를 지정하여 원하는 부분만 추출하는 등 다양한 방식으로 활용할 수 있습니다.

## 관련 링크

- [Golang 공식 사이트](https://golang.org/)
- [Go 언어 튜토리얼](https://golangbot.com/learn-golang-series/)
- [Go 언어 명세서](https://golang.org/ref/spec)