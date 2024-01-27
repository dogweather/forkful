---
title:                "정규 표현식 활용하기"
date:                  2024-01-19
html_title:           "Arduino: 정규 표현식 활용하기"
simple_title:         "정규 표현식 활용하기"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
정규 표현식은 문자열에서 패턴을 찾고 내용을 처리하는 강력한 방법입니다. 프로그래머들은 데이터 검증, 검색, 텍스트 변환 등 다양한 작업을 위해 정규 표현식을 사용합니다.

## How to: (방법)
```Go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	// 정규 표현식 컴파일
	re := regexp.MustCompile(`\d+`)

	// 문자열에서 숫자 찾기
	str := "주문번호: 123456, 제품코드: 654321"
	matches := re.FindAllString(str, -1)
	
	// 결과 출력
	fmt.Println(matches) // ["123456", "654321"]
}
```

## Deep Dive (심화 학습)
1. **역사적 맥락**: 1950년대에 소개된 이후, 정규 표현식은 컴퓨터 과학과 텍스트 처리에서 중요한 도구가 되었습니다.
2. **대안**: 정규 표현식 외에도, 문자열 처리를 위해 문자열 함수 라이브러리나 파싱 라이브러리를 사용할 수 있습니다.
3. **실행 세부 정보**: Go 언어는 `regexp` 패키지를 통해 정규 표현식을 지원하며, RE2 엔진을 사용해 실행 속도와 보안을 보장합니다.

## See Also (참고 자료)
- Go 정규 표현식 패키지: https://pkg.go.dev/regexp
- Go by Example의 정규 표현식 예제: https://gobyexample.com/regular-expressions
- 정규 표현식 연습 사이트: https://regex101.com/
