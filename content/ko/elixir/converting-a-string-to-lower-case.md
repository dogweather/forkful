---
title:                "Elixir: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜?

문자열을 소문자로 변환하는 것이 왜 중요한지 아시나요? 소문자로 변환하면 텍스트 처리와 비교 작업 등 여러 가지 프로그래밍 작업에서 유용합니다. 또한, 영문 대소문자를 구별하지 않는 경우에도 소문자로 변환하여 일관된 결과를 얻을 수 있습니다.

## 어떻게 하나면 될까요?

문자열을 소문자로 변환하는 방법은 간단합니다. Elixir에서는 `String.downcase/1` 함수를 사용하면 됩니다. 이 함수는 문자열을 소문자로 변환한 새로운 문자열을 반환합니다. 아래는 실제 코딩 예제와 그 결과입니다.

```Elixir
iex> String.downcase("HELLO")
"hello"
```

## 깊게 파고들어보기

문자열을 소문자로 변환하는 과정에서 내부적으로 어떤 일이 일어나는지 궁금하신가요? Elixir에서는 문자열을 소문자로 변환하기 전에 문자 하나씩 순회하면서 대문자를 소문자로 바꾸는 작업을 합니다. 이 과정에서 `Unicode` 모듈을 사용하여 문자의 대소문자를 비교하고 변환합니다.

## 더 알아보기

이외에도 Elixir에서 문자열을 다루는 다양한 함수들이 있습니다. `String.upcase/1` 함수를 이용하면 반대로 소문자를 대문자로 변환할 수 있으며, `String.capitalize/1` 함수를 이용하면 문자열의 첫 글자만 대문자로 변환할 수 있습니다. 더 많은 내용을 알고 싶다면 아래 링크들을 참고해보세요!

## 관련 링크

- Elixir 공식 문서 - [String 모듈](https://hexdocs.pm/elixir/String.html)
- [유니코드 관련 정보](https://unicode.org/)