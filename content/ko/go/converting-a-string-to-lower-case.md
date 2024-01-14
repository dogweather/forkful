---
title:    "Go: 문자열 소문자로 변환하기"
keywords: ["Go"]
---

{{< edit_this_page >}}

# 왜

문자열을 소문자로 변환하는 것의 이점은 일반적으로 대소문자 구분 없는 문자열 검색이나 비교를 수월하게 만들어줍니다.

## 방법

```Go
func toLowerCase(str string) string {
    result := ""
    for _, char := range str {
        if char >= 'A' && char <= 'Z' {
            result += string(char + 32)
        } else {
            result += string(char)
        }
    }
    return result
}

fmt.Println(toLowerCase("Hello World"))
```

출력 결과:

`hello world`

## 깊게 파헤치기

Go에서 문자열을 소문자로 변환하는 방법은 여러 가지가 있지만, 가장 간단한 방법은 `if`문과 `for`문을 이용하는 것입니다. `string` 타입은 불변이기 때문에, 새로운 문자열을 만들기 위해 `rune`이라는 타입을 사용합니다. `rune`은 유니코드 문자를 표현하는 데 사용되는 타입으로, `for`문을 이용해 문자열을 순회하면서 각 문자를 `rune`타입으로 변환하고, 대문자라면 `+32`를 해서 소문자로 변환합니다. 그 외에는 그대로 `result`에 추가해 최종적으로 소문자로 변환된 문자열을 반환합니다.

# 참고 자료

- [Effective Go – The Go Programming Language](https://golang.org/doc/effective_go.html#conversions)
- [Strings, bytes, runes and characters in Go - Golang basics](https://www.golangprograms.com/golang-string-operations.html)