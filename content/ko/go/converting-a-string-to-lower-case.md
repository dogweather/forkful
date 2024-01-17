---
title:                "문자열을 소문자로 변환하기"
html_title:           "Go: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 무엇이고 왜?

문자열을 소문자로 변환하는 것은 문자열을 대소문자 구분 없이 비교하거나 일괄적으로 포맷팅할 때 유용합니다. 프로그래머들은 이를 사용하여 더욱 효율적으로 코드를 작성할 수 있습니다.

## 방법:

```Go
// strings.ToLower 함수를 사용하여 문자열을 소문자로 변환합니다.
str := "HELLO WORLD"
lowStr := strings.ToLower(str)

fmt.Println(lowStr)
```

**출력:** hello world

## 깊게 들어가보면:

- **역사적 배경:** 문자열을 소문자로 변환하는 기능은 컴퓨터에서 문자를 표현하는 방식이나 문자 집합의 변화와 함께 계속해서 발전해왔습니다.

- **대체 기능:** 다른 프로그래밍 언어에서도 문자열을 소문자로 변환하는 기능을 지원합니다. 예를 들어, Java에서는 `toLowerCase()` 메서드를 제공합니다.

- **구현 세부사항:** Go 언어에서 `strings.ToLower()` 함수는 문자열을 소문자로 변환하기 위해 모든 문자를 스캔하여 ASCII 범위에서 소문자로 변환하는 방식을 사용합니다.

## 참고 자료:

- [Go 언어 공식 문서: strings.ToLower 함수](https://golang.org/pkg/strings/#ToLower)
- [Java 공식 문서: toLowerCase 메서드](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toLowerCase--)
- [유니코드와 문자 집합](https://ko.wikipedia.org/wiki/%EC%9C%A0%EB%8B%88%EC%BD%94%EB%93%9C%EC%99%80_%EB%AC%B8%EC%9E%90_%EC%A7%91%ED%95%A9)