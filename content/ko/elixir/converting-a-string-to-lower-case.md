---
title:                "문자열을 소문자로 변환하기"
html_title:           "Elixir: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜

스트링을 소문자로 변환하는 것은 일반적으로 프로그래밍에서 텍스트 처리를 할 때 유용합니다. 데이터를 정렬하거나 비교하는 등의 작업에서 소문자로 통일하면 불필요한 오류를 방지하고 효율적인 작업을 할 수 있습니다.

## 하우 투

### 파이프라인 연산자를 사용하는 방법

소문자로 변환할 문자열 변수를 파이프라인 연산자(|)를 사용하여 String.downcase/1 함수에 넘겨줍니다. 이 함수는 주어진 문자열을 모두 소문자로 변환하여 새로운 문자열을 반환합니다.

```Elixir
name = "JOHN"
name |> String.downcase()    # "john"
```

### 패턴 매칭을 사용하는 방법

String.downcase/1 함수는 패턴 매칭을 사용하여 문자열을 소문자로 변환할 수도 있습니다. 이 경우 변환된 문자열이 바로 변수에 할당되기 때문에 따로 할당 작업을 하지 않아도 됩니다.

```Elixir
name = "JOHN"
String.downcase(name)    # "john"
```

## 딥 다이브

파이썬과 같은 다른 언어에서는 문자열을 변환하기 전에 변수 내용이 변경되지 않도록 완전히 새로운 객체를 만들어서 처리합니다. 하지만 Elixir에서는 문자열을 변경할 수 없기 때문에 변환 후에도 원본 변수의 내용은 변하지 않습니다. 이는 함수형 프로그래밍에서 immutable 데이터를 사용하는 것과 관련이 있습니다.

## 더 알아보기

[Elixir String Module](https://hexdocs.pm/elixir/String.html)

[Elixir Pipes](https://elixir-lang.org/getting-started/enumerables-and-streams.html#pipes)

## 연관 글

- [Elixir에서 문자열 다루기](https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html)
- [Elixir의 함수형 프로그래밍 개념](https://elixir-lang.org/getting-started/functional-programming.html)