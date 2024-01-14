---
title:                "Elixir: 문자열의 길이 찾기"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열의 길이를 찾게 되는 이유는 데이터를 다룰때 중요한 부분 중 하나입니다. 프로그래머는 자주 다양한 유형의 데이터를 다루게 되는데, 그 중에는 문자열 또한 포함됩니다. 따라서 문자열의 길이를 정확히 파악하는 것은 중요한 스킬입니다.

## 방법

``` Elixir
# 문자열의 길이 구하기
input = "안녕하세요"
IO.puts String.length(input)

# 출력결과
5
```

위의 예제에서 볼 수 있듯이, Elixir에서는 `String.length` 함수를 사용하여 간단하게 문자열의 길이를 구할 수 있습니다. 또한 입력한 문자열 내에 공백 또한 길이에 포함되므로 유의해야 합니다.

## 깊이 파고들기

문자열의 길이를 구하는 방법은 내부적으로 어떻게 작동할까요? Elixir에서는 문자열을 바이트로 저장하므로, `String.length` 함수는 문자열의 바이트 수를 반환합니다.

예를 들어, "안녕하세요"라는 문자열의 경우 한글은 3바이트로 되어있기 때문에 `String.length` 함수는 5를 반환합니다. 이는 각 문자를 나타내는 코드 포인트가 2바이트이고, 마지막 공백은 1바이트로 이루어져 있기 때문입니다. 따라서 문자열의 길이를 구할 때에는 바이트 수를 주의 깊게 살펴보는 것이 중요합니다.

## 관련 정보

- [Elixir 문서](https://hexdocs.pm/elixir/Kernel.html#String.length/1)
- [문자열 다루는 방법](https://elixirschool.com/kr/lessons/basics/string/)
- [바이트와 코드 포인트의 차이점](https://medium.com/@joaodlf/understanding-bytes-code-points-and-graphemes-in-elixir-and-erlang-60d9b323fd46)

## 참고 자료

- [Elixir 공식 문서](https://elixir-lang.org/)
- [Elixir School](https://elixirschool.com/kr/)
- [Elixir 커뮤니티 사이트](https://elixir-lang.org/community/)