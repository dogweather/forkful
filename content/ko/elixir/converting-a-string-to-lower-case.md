---
title:                "Elixir: 문자열을 소문자로 변환하기"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜?

문자열을 소문자로 변환하는 이유는 개발자가 주어진 입력을 다루기 쉽게 만들기 위해서입니다.

## 어떻게 하나요?

```Elixir
iex> String.downcase("HELLO WORLD")
"hello world"
```

```Elixir
iex> String.downcase("Elixir is AWESOME")
"elixir is awesome"
```

변환된 문자열은 원본 문자열과 다른 새로운 문자열로 반환됩니다. 따라서 이와 관련된 작업을 수행하려면 변환된 값을 변수에 할당해야 합니다.

```Elixir
iex> original_string = "BeWare oF CapItAls"
"BeWare oF CapItAls"

iex> transformed_string = String.downcase(original_string)
"beware of capitals"

iex> original_string #원본 문자열은 변화하지 않습니다.
"BeWare oF CapItAls"

```

## 깊게 파고들기

문자열을 소문자로 변환하는 `String.downcase/1` 함수는 Elixir의 내장 함수입니다. 이 함수는 문자열에 있는 모든 문자를 소문자로 변환하고, 원본 문자열에는 아무런 영향을 미치지 않습니다.

하지만 이 함수를 사용할 때에는 주의해야 할 점이 있습니다. `String.downcase/1` 함수는 Unicode 문자를 처리할 때 바이트 수준에서의 작업이 아닌 문자 수준에서의 작업을 수행합니다. 따라서 변환된 문자열의 길이는 원본 문자열의 길이와 다를 수 있습니다.

## 관련 링크

- [String.downcase/1 문서](https://hexdocs.pm/elixir/String.html#downcase/1)
- [String 모듈 문서](https://hexdocs.pm/elixir/String.html)