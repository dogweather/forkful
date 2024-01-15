---
title:                "문자열 대문자로 변환하기"
html_title:           "Elixir: 문자열 대문자로 변환하기"
simple_title:         "문자열 대문자로 변환하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열을 대문자로 바꾸는 방법에 대해 알기 위해서는 Elixir 프로그래밍 언어를 알아야 합니다. 이는 Elixir에서 문자열을 조작하는 중요한 기술 중 하나이기 때문입니다.

## 코딩 예제

```Elixir
string = "hello world"
IO.puts String.upcase(string)
```

출력:

```
HELLO WORLD
```

```Elixir
string = "I love elixir"
IO.puts String.upcase(string)
```

출력:

```
I LOVE ELIXIR
```

## 깊이 알아보기

Elixir에서 문자열을 조작하는 기능은 기본적으로 제공되지 않습니다. 다만, 라이브러리 함수인 `String.upcase/1`을 사용하면 간단하게 문자열을 대문자로 바꿀 수 있습니다. 또한, 대소문자를 구분하지 않는 `String.downcase/1` 함수도 있습니다.

## 더 알아보기

Elixir에서 제공하는 문자열 조작 함수에 대해 더 알아보려면 [String 모듈 문서](https://hexdocs.pm/elixir/String.html)를 참고하시기 바랍니다.

## 같이 참고하기

- [Elixir Strings - learnbeautiful](https://www.learnbeautiful.org/blog/elixir-strings/)
- [Elixir School - Strings](https://elixirschool.com/lessons/basics/string/)