---
title:                "Go: 부분 문자열 추출하기"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜

문자열에서 부분 문자열을 추출하는 것은 프로그래밍에서 일반적으로 사용되는 중요한 기술입니다. 부분 문자열을 추출하는 것은 문자열에서 특정한 부분을 선택하여 원하는 작업을 수행하기 위해 필요합니다. 

## 추출하는 방법

Go 언어에서는 "Strings" 패키지를 사용하여 부분 문자열을 추출할 수 있습니다. 예를 들어, 다음과 같은 코드를 사용하여 문자열에서 3번째부터 6번째까지의 부분 문자열을 추출할 수 있습니다.

```Go
str := "Go 언어는 쉽고 강력합니다."
substr := str[2:6]
fmt.Println(substr) 

// 결과: 언어는
```

위의 코드에서 `str` 변수는 원본 문자열이고 `substr` 변수는 추출한 부분 문자열입니다. 부분 문자열은 항상 원본 문자열의 일부분일 뿐이므로 원본 문자열이 변경되더라도 부분 문자열은 변경되지 않습니다.

## 깊이 파헤치기

Go 언어에서 부분 문자열을 추출하는 방법과 관련된 다양한 기능들이 있습니다. 예를 들어, `strings.Index()` 함수를 사용하여 문자열에서 특정 문자열이 처음 등장하는 위치를 찾을 수 있고, `strings.Replace()` 함수를 사용하여 문자열 내의 특정 문자열을 다른 문자열로 대체할 수 있습니다. 

부분 문자열 추출과 관련된 더 깊은 내용을 학습하려면 Go 언어 공식 문서를 참고하는 것이 좋습니다. 또한, `strings` 패키지 외에도 `regexp` 패키지를 사용하여 정규식을 이용해 문자열에서 부분 문자열을 추출할 수도 있습니다.

## 더 찾아보기

- [Go 언어 공식 문서](https://golang.org/pkg/strings/)
- [정규식을 이용한 문자열 추출 예제](https://yourbasic.org/golang/extract-substring-from-string/)
- [Go 언어 정규식 패키지 문서](https://golang.org/pkg/regexp/)