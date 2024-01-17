---
title:                "문자열 대문자로 변환하기"
html_title:           "Gleam: 문자열 대문자로 변환하기"
simple_title:         "문자열 대문자로 변환하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 무엇인가요? 
문자열을 대문자로 바꾸는 것은 무엇일까요? 프로그래머들이 이것을 왜 하는지 알아봅시다.

대문자로 바꾸는 것은 문자열의 모든 글자를 대문자로 만드는 것을 말합니다. 이것을 하는 이유는 주로 사용자의 입력을 표준화하거나, 데이터를 더 쉽게 처리하기 위해서 입니다.

## 방법:
Gleam 언어를 사용하여 문자열을 대문자로 바꾸는 방법은 다음과 같습니다:

```gleam
my_string = "hello"
capitalized_string = String.to_upper(my_string)

IO.inspect(capitalized_string) // "HELLO"
```

## 깊이 파고들기:
대문자로 바꾸는 것은 과거에는 매우 어려웠습니다. 하지만 지금은 다양한 함수나 라이브러리를 사용하여 쉽게 할 수 있습니다. 다른 언어에서는 `toupper()` 함수를 사용하곤 합니다. 또한 lowercase, titlecase 와 같은 다양한 변형도 존재합니다.

또한 코드 내부적으로 문자를 바꾸지 않고 원본 문자열을 유지하는 함수도 존재합니다. 이 방법이 더 효율적일 수 있으며, 필요한 경우에 원본 문자열을 재사용할 수 있습니다.

## 함께 보기:
관련 정보를 더 찾고 싶다면 아래 링크들을 확인해보세요.
- [Gleam Strings 공식 문서](https://gleam.run/documentation/guide/strings.html)
- [Gleam 라이브러리](https://github.com/gleam-lang/gleam_stdlib/tree/master/lib/gleam)