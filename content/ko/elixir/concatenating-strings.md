---
title:                "문자열 연결하기"
html_title:           "Elixir: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
문자열을 연결하는 것은 문자열의 각 구성 요소를 결합하여 하나의 문자열로 만드는 것을 말합니다. 이것은 프로그래머가 다양한 문자열을 하나로 합쳐야 할 때 유용합니다.

## 방법:
```Elixir
string1 = "Hello "
string2 = "World!"

IO.puts string1 <> string2
```
```
Hello World!
```
```Elixir
name = "John"
greeting = "Hi "
age = 25

IO.puts greeting <> name <> ", you are " <> age <> " years old."
```
```
Hi John, you are 25 years old.
```

## 깊이 파고들기:
(1) 문자열 연결은 문자열 처리에서 일반적으로 사용되는 기술입니다. (2) Elixir에서는 <> 연산자가 문자열을 연결하는 데 사용됩니다. 실행 속도 측면에서, 한 번에 여러 개의 문자열을 연결하는 것보다 개별 문자열을 각각 연결하는 것이 더 효율적입니다.

## 참고:
- <https://elixir-lang.org/getting-started/string-interpolation-and-output.html>