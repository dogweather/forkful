---
title:    "Elixir: 문자열 대문자화"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열을 대문자로 변환하는 것이 어떤 이유로 중요한지 아시나요? 만약 당신이 Elixir 프로그래밍을 배우고 있다면, 이것이 앞으로 많은 도움이 될 것입니다. 이번 글에서는 왜 문자열을 대문자로 변환하는 것이 중요한지 알아보고, 어떻게 하면 쉽게 이것을 할 수 있는지 살펴보도록 하겠습니다.

## 방법

먼저, 간단한 문자열을 `string` 변수에 저장합니다. 그리고 `String.upcase/1` 함수를 사용하여 문자열을 대문자로 변환합니다.

```Elixir
string = "hello"
String.upcase(string)
```

이제 변수 `string`은 `"HELLO"`로 변환됩니다.

## 더 깊이 파고들어보기

문자열을 대문자로 바꾸는 것은 간단하지만, 여러분이 이것을 쓰는 이유를 알고 싶을 것입니다. 우리가 컴퓨터 프로그래밍에서 문자열을 다루는 이유는 다양합니다. 예를 들어, 사용자의 입력을 검증하기 위해서는 입력된 문자열을 모두 소문자로 바꾼 다음 비교해야 합니다. 또는 데이터베이스에서 검색할 때 사용자 입력과 일치하는지 확인하기 위해서도 문자열을 일치하는 대소문자로 변환해야 합니다.

## 관련 글

- [Elixir String Module Documentation](https://hexdocs.pm/elixir/String.html)
- [Manipulating Strings in Elixir](https://elixirschool.com/en/lessons/basics/binary-and-string/#manipulating-strings)
- [Why You Should Learn Elixir](https://www.codementor.io/codeforgeek/why-you-should-learn-elixir-f1jo2pu5f)