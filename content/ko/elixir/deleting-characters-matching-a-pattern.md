---
title:                "Elixir: 패턴과 일치하는 문자 삭제하기"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜

주어진 패턴과 일치하는 문자를 삭제하는 작업을 수행하는 이유는 여러 가지가 있을 수 있습니다. 예를 들어, 특정 문자열에서 불필요한 공백을 제거하거나, 특정 패턴을 가진 단어를 필터링하는 등의 작업을 수행할 수 있습니다. 이러한 작업을 통해 코드의 가독성을 높이거나, 데이터 정제 작업을 수행할 수 있습니다.

## 방법

우선, `Regex` 모듈을 사용하여 문자열에서 패턴을 찾은 후, `String.replace/3` 함수를 사용하여 해당 패턴과 일치하는 문자를 삭제할 수 있습니다. 아래는 간단한 예시 코드입니다.

```elixir
input = "Hello! 안녕하세요!"
pattern = ~r/[!#?]/  # 입력된 문자열에서 !, #, ? 문자를 찾습니다.
output = String.replace(input, pattern, "")  # 입력된 문자열에서 발견된 패턴과 일치하는 문자를 제거합니다.
# 출력: "Hello 안녕하세요"
```

또 다른 방법으로는 `String.replace/4` 함수를 사용하는 것입니다. 이 함수는 검색 패턴과 일치하는 모든 문자열을 찾아서 특정 문자열로 대체할 수 있습니다. 예를 들어, 아래 코드에서는 입력된 문자열에서 모든 숫자를 "X"로 대체합니다.

```elixir
input = "12345"
pattern = ~r/\d/  # 입력된 문자열에서 숫자를 찾습니다.
output = String.replace(input, pattern, "X")  # 숫자를 "X"로 대체합니다.
# 출력: "XXXXX"
```

## 딥 다이브

이제 좀 더 깊게 알아보도록 하겠습니다. `Regex` 모듈의 `replace/3` 함수는 입력된 문자열에서 검색 패턴과 일치하는 첫 번째 문자만을 대체합니다. 따라서 입력된 문자열에 동일한 패턴이 여러 개 있다면, 사용자가 원하는 대로 대체 되지 않을 수 있습니다. 이에 대한 예외 처리를 해주는 것이 좋습니다.

또한, 입력된 문자열이 아스키 문자열이 아닌 경우도 예외 처리를 해주어야 합니다. 이는 문자열의 인코딩과 관련이 있습니다. 예를 들어, 한글이 포함된 문자열을 변경하는 경우 아래와 같은 에러가 발생할 수 있습니다.

```
** (ArgumentError) argument error
    :erlang.binary_to_list("안")
```

따라서 문자열의 인코딩을 명시적으로 지정해주어야 합니다. 예를 들어, 위의 예제 코드를 아래와 같이 수정할 수 있습니다.

```elixir
input = "Hello 안녕하세요!"
pattern = ~r/[!#?]/  # 입력된 문자열에서 !, #, ? 문자를 찾습니다.
output = String.replace(input, pattern, "", [encoding: :utf8])  # 입력된 문자열에서 발견된 패턴과 일치하는 문자를 제거합니다.
# 출력: "Hello 안녕하세요"
```

## 참고

- [`Regex` 모듈 공식 문서](https://hexdocs.pm/elixir/Regex.html)
- [Elixir School: Pattern Matching](https://elixirschool.com/lessons/basics/pattern-matching/)
- [Learn Elixir: Regular Expressions](https://www.learnelixir.tv/lessons/basics-regular-expressions)