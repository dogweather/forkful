---
title:                "패턴과 일치하는 문자 삭제"
html_title:           "Go: 패턴과 일치하는 문자 삭제"
simple_title:         "패턴과 일치하는 문자 삭제"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# 뭐고 왜?

문자 패턴과 맞는 문자들을 삭제하는 것은 프로그래머들이 반복적으로 긴 문자열을 다룰 때에 도움되는 간단한 작업입니다. 예를 들어, 특정 패턴을 가진 파일에서 사용하지 않는 라이브러리를 삭제하는 경우에 사용할 수 있습니다.

## 방법:

```Go
// 예시 코드:
func deletePattern(pattern string, text string) string {  // 패턴을 수신하고 원본 텍스트를 반환하는 함수를 정의합니다.
	re := regexp.MustCompile(pattern)  // 패턴을 정규표현식으로 변환합니다.
	return re.ReplaceAllString(text, "")  // 문자열에서 패턴과 일치하는 부분을 모두 제거합니다.
}
```
```Go
// 샘플 입력:
fmt.Println(deletePattern("o", "Hello World!"))

// 예상 결과:
Hell Wrld!
```

## 깊은 무지:

### 역사적 맥락:
문자열에서 특정 패턴을 삭제하는 기능은 오래 전부터 프로그래밍 언어와 도구에서 사용되어 왔습니다. 예를 들어, C 언어에서는 문자열을 검색하고 삭제하는 함수가 이미 포함되어 있었습니다.

### 대안:
Go 언어에서는 문자열을 다루는 기능과정에서도 삭제 작업을 수행할 수 있습니다. 예를 들어, strings 패키지에서 제공하는 Replace 함수를 사용하는 것도 좋은 대안입니다.

### 구현 세부사항:
Go 언어에서는 문자열에서 패턴을 삭제하는 등의 작업을 위해 정규표현식을 사용합니다. 이는 문자열을 조작하기 위한 강력한 도구로써 여러 종류의 패턴을 손쉽게 삭제할 수 있게 해줍니다.

## 더보기:

- [Go 언어 공식 문서](https://golang.org/pkg/regexp/)에서 문자열과 정규표현식에 대한 더 자세한 내용을 확인할 수 있습니다.
- [Go 언어 공식 문서](https://golang.org/pkg/strings/)에서 strings 패키지에 대한 정보를 참고할 수 있습니다.