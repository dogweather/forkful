---
title:                "문자열을 소문자로 변환하기"
html_title:           "Gleam: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜

문자열을 소문자로 변환하는 것에 관심이 있을 수 있습니다. 예를 들어, HTML 양식에서 사용자가 입력한 값을 처리할 때 대문자나 소문자에 구애받지 않도록 하기 위해서입니다.

## 어떻게 하나요?

```Gleam
let string = "Hello World"
let lower_case = String.to_lower_case(string)
```

`String.to_lower_case` 함수를 사용하여 문자열을 소문자로 변환할 수 있습니다. 위의 코드에서는 "Hello World"라는 문자열을 소문자로 변환한 뒤 `lower_case` 변수에 저장되어 있습니다. 코드를 실행하면 `lower_case` 변수에는 "hello world"라는 값이 저장되게 됩니다.

```Gleam
let string = "저를 소문자로 바꿔 주세요"
let lower_case = String.to_lower_case(string)
```

위의 코드에서는 한글 문자열도 소문자로 변환할 수 있습니다. 또한, `String.to_upper_case` 함수를 사용하면 문자열을 대문자로 변환할 수도 있습니다.

## 깊게 살펴보기

`String.to_lower_case` 함수를 사용하면 입력된 문자열에 포함된 모든 글자를 소문자로 바꿀 수 있습니다. 이 함수는 영문 대문자, 소문자, 한글 모두를 변환할 수 있습니다. 또한, 특수문자나 숫자는 변환하지 않기 때문에 HTML 태그와 같은 문자열을 처리할 때 유용하게 사용될 수 있습니다.

## 다른 글들도 보세요

- [Gleam 공식 문서](https://gleam.run/documentation/#string) - Gleam의 모든 내장 함수에 대한 자세한 설명을 볼 수 있습니다.
- [Elixir vs Gleam: 뭐가 다를까?](https://gleam.run/posts/elixir_vs_gleam/) - Elixir와 Gleam의 차이점을 알아보는 재미있는 글입니다.