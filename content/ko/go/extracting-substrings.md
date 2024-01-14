---
title:                "Go: 부분 문자열 추출하기"
simple_title:         "부분 문자열 추출하기"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/extracting-substrings.md"
---

{{< edit_this_page >}}

#︎ 왜 substring 추출에 관심을 가져야 할까요?

Substring 추출은 많은 프로그래밍언어에서 중요한 작업 중 하나입니다. 이는 문자열에서 원하는 부분을 추출하는 데에 사용됩니다. Go에서도 마찬가지입니다. Substring 추출을 통해 문자열 처리를 더욱 쉽게 할 수 있습니다.

## 어떻게 substring을 추출할 수 있을까요?

Go에는 strings 패키지에 내장된 기능을 사용하면 간단하게 substring을 추출할 수 있습니다. 아래는 기본적인 코드 예제와 함께 substring 추출하는 방법입니다.

```Go
// 예제 문자열
str := "안녕하세요, Go 언어를 배우는 것은 정말 즐거운 일입니다."

// 인덱스 2부터 5까지 substring 추출
substr1 := str[2:5]
fmt.Println(substr1) // 결과: 녕하세

// 맨 뒤에서 3번째 글자부터 마지막까지 substring 추출
substr2 := str[len(str)-3:]
fmt.Println(substr2) // 결과: 일입니다.
```

## 깊게 들어가보세요

- 문자열을 처리하는 데에 Go에서는 보다 유용한 기능들이 많이 제공됩니다.
- strings 패키지에서는 Contains, Index, Count, Replace 등의 기능도 제공합니다.
- Java 와 달리 Go에서는 문자열에서 원하는 부분을 추출할 때마다 새로운 문자열을 생성하지 않습니다. 이를 잘 활용하면 성능을 향상시킬 수 있습니다.

## 또 다른 정보는 여기를 참조하세요

- [Go 공식문서 - Strings 패키지](http://golang.org/pkg/strings/)
- [The Go Blog - Strings](https://blog.golang.org/strings)
- [Go Strings 패키지 사용법](https://www.dotnetperls.com/string-go)

#︎ 또 다른 정보는 여기를 참조하세요