---
title:                "문자열 연결하기"
html_title:           "Arduino: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## 무엇인가? 왜 사용하는가?

문자열 연결(concatenating strings)은 다양한 문자열을 하나로 결합하는 일을 말합니다. 이를 통해 우리는 동적으로 생성된 내용을 표현하거나, 복잡한 메시지를 구성할 수 있습니다.

## 어떻게 사용하는가?

```Elixir
# 두 문자열 연결하기
name = "John"
greeting = "Hello, " <> name
IO.puts(greeting)  # "Hello, John" 출력
```

```Elixir
# 여러 문자열 합치기
parts = ["This", "is", "a", "sentence."]
sentence = Enum.join(parts, " ")
IO.puts(sentence)  # "This is a sentence." 출력
```

## 깊게 알아보기

Elixir의 문자열 연결 방식은 언어의 불변성(Immutable) 성질에 기반하고 있습니다. 문자열이 불변하기 때문에 새 문자열을 생성해내는 방식으로 연결이 이루어집니다.

역사적으로, Elixir에서는 `<>` 연산자를 통해 문자열을 직접적으로 연결하였습니다. 이 방법 외에도, `Enum.join/2` 함수를 활용하여 많은 문자열들을 결합하는 방법도 존재합니다.

## 참고 자료

- [Elixir 공식 문서: 문자열 연결](https://hexdocs.pm/elixir/String.html#concat/2)
- [Elixir 학습자료: 문자열 핸들링](https://elixirschool.com/en/lessons/basics/strings/)
- [Elixir 튜토리얼: Enum](https://elixir-lang.org/getting-started/enum-and-streams.html)