---
title:                "Elixir: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열의 길이를 구하는 것은 프로그램을 작성할 때 매우 중요합니다. 예를 들어 사용자의 입력을 검증하거나 특정 문자열을 처리할 때 길이를 확인해야 할 수 있습니다. 이러한 상황에서 문자열의 길이를 구하는 방법에 대해 알아보겠습니다.

## 어떻게 하나요

문자열의 길이를 구하는 가장 간단한 방법은 `String.length/1` 함수를 사용하는 것입니다. 이 함수는 문자열의 길이를 정수 형태로 반환합니다.

```Elixir
string = "Hello World"
String.length(string) #=> 11
```

이 외에도 `length/1` 함수는 모든 유니코드 문자를 지원하므로 다국어 문자열에 대해서도 올바르게 동작합니다.

또 다른 방법은 문자열을 리스트로 변환한 후 `length/1` 함수를 사용하는 것입니다. 이 방법은 문자열 내에 있는 모든 문자를 카운트할 수 있으며, 유니코드 문자도 제대로 처리할 수 있습니다.

```Elixir
string = "안녕하세요"
string |> String.codepoints() |> length() #=> 5
```

## 깊이 파고들기

문자열의 길이를 구하는 방법은 간단하지만, 이와 관련하여 몇 가지 유용한 함수를 알아봅시다.

- `String.trim/1` 함수는 문자열 앞뒤에 있는 공백을 제거한 후 문자열의 길이를 반환합니다.
- `String.graphemes/1` 함수는 유니코드 그래프로 이루어진 리스트를 반환하므로, 이를 조합하여 정확한 문자열의 길이를 구할 수 있습니다.
- `String.codepoints/1` 함수는 문자열을 유니코드 코드 포인트로 이루어진 리스트로 변환하므로, 이를 이용해 문자열의 길이를 구할 수 있습니다.

이러한 함수들을 응용하면 문자열의 길이를 유연하게 다룰 수 있습니다.

## 참고하기

- Elixir 문서: https://hexdocs.pm/elixir/String.html#length/1
- ElixirSchool: https://elixirschool.com/lessons/basics/built-in-types/#strings
- Learn X in Y minutes: https://learnxinyminutes.com/docs/kr/elixir-kr/