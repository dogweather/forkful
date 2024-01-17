---
title:                "정규 표현식 사용하기"
html_title:           "Go: 정규 표현식 사용하기"
simple_title:         "정규 표현식 사용하기"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

# 이게 뭐고, 왜 해야 할까요?

정규 표현식은 문자열에서 패턴을 찾거나 조작하기 위한 도구입니다. 프로그래머들은 이것을 사용하는 이유는 간단합니다. 정규 표현식은 복잡한 문자열 작업을 더 쉽고 간단하게 만들어주기 때문입니다.

## 사용 방법:

```Go
// 패키지 가져오기
import "regexp"

// 정규 표현식 패턴 정의
pattern := "a.c"

// 패턴과 일치하는 첫 번째 문자열 찾기
match := regexp.MustCompile(pattern).FindString("abc")
fmt.Println(match) // 출력값: abc

// 패턴과 일치하는 모든 문자열 찾기
matches := regexp.MustCompile(pattern).FindAllString("abracadabra", -1)
fmt.Println(matches) // 출력값: [abc ac]

```

## 깊은 곳으로:

정규 표현식은 1950년대에 개발된 존 프러트(John Pinkerton)의 아이디어에서 시작되었습니다. 다른 대안으로는 문자열 패턴 매칭을 위한 다양한 라이브러리나 툴이 존재하지만, 정규 표현식은 가장 많이 사용되는 방법 중 하나입니다. Go에서의 정규 표현식 구현 방식은 DFA(결정적 유한 상태 자동기계) 기반으로 작성되어 있습니다.

## 관련 자료:

- 정규 표현식 기본 개념: https://www.regular-expressions.info/
- Go 공식 문서: https://golang.org/pkg/regexp/
- 정규 표현식 테스트: https://regex101.com/