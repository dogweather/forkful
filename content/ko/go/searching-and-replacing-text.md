---
title:                "텍스트 검색 및 교체"
html_title:           "Elixir: 텍스트 검색 및 교체"
simple_title:         "텍스트 검색 및 교체"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

텍스트 검색 및 교체는 특정 문자열을 찾아 다른 문자열로 바꾸는 것입니다. 이는 코드 중복을 줄이고, 값 변환, 패턴 일치, 데이터 정렬 등의 작업을 빠르게 처리할 수 있게 해줍니다.

## 어떻게 해야하나요:

텍스트 검색 및 교체는 Go 언어에서 간단합니다. 아래 코드 예제를 보세요:

```Go
package main
import "strings"
import "fmt"

func main() {
    sample := "Hello, World of Go programming!"
    fmt.Println(strings.Replace(sample, "World", "Universe", -1))
}
```

`strings.Replace` 함수가 작동하는 모드:
처음 두 인자는 원본 문자열 및 검색할 문자열이며, 세 번째 인자는 교체할 문자열입니다. 마지막 인자는 교체 횟수입니다. -1로 설정하면 모든 일치 항목을 교체합니다.

코드를 실행하면 다음과 같은 결과를 얻을 수 있습니다:

```Go
Hello, Universe of Go programming!
```

## 깊게 들여다 보기:

텍스트 검색 및 교체는 컴퓨팅의 보편적인 작업으로, 텍스트 편집, 스크립팅 언어, 그리고 프로그래밍 언어의 기본 특징 중 하나입니다.

`string.Replace` 외에도 정규 표현식을 사용해서 더 복잡한 패턴을 검색 및 교체할 수 있고, `bytes.Replace` 또는 `bytes.ReplaceAll` 등의 함수를 사용해 바이트 슬라이스에서 검색 및 교체를 수행할 수 있습니다.

이러한 함수들은 내부적으로 `bytes.Replace` 및 `bytes.Equal` 등의 함수를 사용하여 문자열 및 바이트 슬라이스에서 패턴을 찾습니다.

## 참고 문헌:

- Go 언어 공식 문서: https://golang.org/
- Go에 대한 자세한 내용은 'The Go Programming Language' 책을 참고하십시오: https://www.gopl.io/
- 깊은 이해를 위해 'Learn Go with Tests'를 참조하십시오: https://quii.gitbook.io/learn-go-with-tests/