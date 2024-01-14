---
title:                "Go: 문자열을 소문자로 변환하기"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜

문자열을 소문자로 변환하는 것이 중요한 이유는 다른 문자열 연산에서 대소문자 구분을 제거하기 쉽고 일관된 데이터 비교를 가능하게 하기 위해서입니다.

## 방법

```Go
func toLower(str string) string {
    lowerStr := strings.ToLower(str)
    return lowerStr
}

func main() {
    input := "Hello, World!"
    output := toLower(input)
    fmt.Println(output)
}
```

출력:
```
hello, world!
```

## 딥 다이브

Go에서는 기본적으로 문자열을 소문자로 변환하는 `ToLower()` 함수를 제공합니다. 이 함수는 해당 문자열의 원래 값을 변경하는 것이 아니라 새로운 문자열을 반환하기 때문에 원본 문자열의 값을 그대로 유지할 수 있습니다. 또한 이 함수는 유니코드 문자를 올바르게 변환하여 다국어 문자열에서도 정확한 결과를 얻을 수 있도록 지원합니다.

## 참고 자료

- Go 공식 문서 (https://golang.org/pkg/strings/)
- "How I Learned to Stop Worrying and Love the Gopher: A Beginner's Guide to Go" (https://blog.golang.org/gopher)