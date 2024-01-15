---
title:                "패턴과 일치하는 문자 삭제하기"
html_title:           "Elixir: 패턴과 일치하는 문자 삭제하기"
simple_title:         "패턴과 일치하는 문자 삭제하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜

누군가가 패턴과 일치하는 문자를 삭제하는 것에 참여하는 이유는 주로 데이터 정제나 문자열 처리를 위해서입니다. 이 작업은 잘못된 정보를 제거하거나 원하는 데이터를 추출하기 위해 필수적입니다.

## 하는 방법

아래는 Elixir에서 문자를 삭제하는 두 가지 방법을 소개합니다.

### 문자열 간단히 삭제하기

```elixir
string = "Hello, world!"
result = String.replace(string, "o", "")
IO.puts(result)

# 출력:
# Hell, wrld!
```

위 예제에서는 `String.replace` 함수를 사용하여 문자열에서 `o`를 찾아 삭제하였습니다. 이를 통해 특정 문자를 간단하게 삭제할 수 있습니다.

### 정규표현식으로 삭제하기

```elixir
string = "I love coding in Elixir! #elixirlang"
result = Regex.replace(~r/[A-Z]/, string, "")
IO.puts(result)

# 출력:
#  love coding in l! #elixirlang
```

정규표현식을 사용하면 더 복잡한 패턴에 따라 문자를 삭제할 수 있습니다. 예를 들어, 위 예제에서는 대문자를 정규표현식으로 찾아 삭제하는 방법을 보여줍니다.

## 깊이 들어가기

이번 섹션에서는 문자를 삭제하기 위한 다양한 함수와 옵션에 대해 알아보겠습니다.

### `delete_at`

`delete_at` 함수는 주어진 위치에 있는 문자를 삭제하는 함수입니다. 예를 들어:

```elixir
string = "Hello, world!"
result = String.delete_at(string, 4)
IO.puts(result)

# 출력:
# Hello world!
```

### `stringify`

`stringify` 옵션은 값을 문자열로 변환해주는 역할을 합니다. 따라서 숫자나 특수문자도 문자로 변환되어 삭제될 수 있습니다. 예를 들어:

```elixir
string = "Hello, world!"
result = String.replace(string, "o", "", stringify: true)
IO.puts(result)

# 출력:
# Hell, wrld!
```

### `all`

`all` 옵션은 패턴과 일치하는 모든 문자를 삭제하는 옵션입니다. 예를 들어:

```elixir
string = "I love coding in Elixir! #elixirlang"
result = Regex.replace(~r/[a-z]/, string, "", all: true)
IO.puts(result)

# 출력:
#     ! #E !
```

## 참고

* [Elixir 공식 문서 - String](https://hexdocs.pm/elixir/String.html)
* [Elixir 공식 문서 - Regex](https://hexdocs.pm/elixir/Regex.html)
* [Pattern matching in Elixir](https://elixir-lang.org/getting-started/pattern-matching.html)