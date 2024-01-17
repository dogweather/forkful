---
title:                "문자열의 보간"
html_title:           "Go: 문자열의 보간"
simple_title:         "문자열의 보간"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열 보간(interpolating a string)이란 무엇인가요? 이는 문자열 내부에 변수 값을 삽입하는 것을 말합니다. 이는 코드를 간결하고 가독성 높게 만들어주며, 변수 값이 동적으로 변경되는 경우 유용합니다. 

## 어떻게 하나요?

```Go
name := "Jane"
age := 25
fmt.Printf("나의 이름은 %s이고, 나이는 %d살입니다.", name, age)
```

위의 예시 코드를 실행하면 "나의 이름은 Jane이고, 나이는 25살입니다."라는 결과가 나옵니다. 변수 값을 `%s`와 `%d` 지시자로 문자열 내부에 넣으면 됩니다. 

## 깊이 있는 정보

- **역사적 맥락**: 보간 기능은 C언어에서 시작되었으며, 이후 다른 프로그래밍 언어에도 적용되었습니다. Go에서도 이를 지원하고 있습니다.
- **대체 방법**: 보간을 대체할 수 있는 방법으로는 문자열 포맷 출력, 문자열 결합 등이 있지만, 가독성과 유지보수 측면에서 보간은 좋은 선택입니다.
- **구현 세부사항**: Go에서 문자열 보간은 `fmt.Sprintf()` 함수를 사용하여 처리됩니다. 해당 함수는 문자열과 변수 값을 받아 문자열 내부에 지시자에 맞게 변수 값을 삽입해주는 역할을 합니다.

## 참고 자료

- [Go 공식문서 - fmt 패키지](https://golang.org/pkg/fmt/)
- [Effective Go - Formatting Units](https://golang.org/doc/effective_go.html#formatting_units)
- [The Beauty of Go - String Interpolation](https://blog.golang.org/string-intepolation)