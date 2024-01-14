---
title:                "Elixir: 문자열의 대문자화"
simple_title:         "문자열의 대문자화"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜

Capitalizing a string은 Elixir 프로그래밍에서 자주 사용되는 기능 중 하나입니다. 이 기능은 문자열의 첫 글자를 대문자로 변환하는 것을 말합니다. 예를 들어, "hello world"라는 문자열을 capitalizing하면 "Hello world"가 됩니다. 이 기능은 프로그램의 외부 입력 값을 처리할 때 유용하게 사용될 수 있습니다. 사용자로부터 입력 받은 문자열의 첫 글자를 대문자로 바꾸어서 보기 좋은 출력 결과를 얻을 수 있기 때문입니다.

## 하기

```Elixir
input = "hello world"
IO.puts String.capitalize(input) #=> "Hello world"
```

위의 코드는 문자열 "hello world"를 capitalizing하는 간단한 예시입니다. 문자열을 String 모듈의 capitalize 함수에 전달하여 첫 글자가 대문자로 바뀐 결과를 얻을 수 있습니다. 또한, 첫 글자가 대문자인 경우에는 변화가 없으므로 안전하게 사용할 수 있습니다.

```Elixir
input = "Elixir"
IO.puts String.capitalize(input) #=> "Elixir"
```

만약 모든 단어의 첫 글자를 대문자로 바꾸고 싶다면 String 모듈의 capitalize_words 함수를 사용할 수 있습니다. 이 함수는 문자열을 단어 별로 쪼개고 각 단어의 첫 글자를 대문자로 변환한 후 다시 합치는 기능을 합니다.

```Elixir
input = "hello world"
IO.puts String.capitalize_words(input) #=> "Hello World"
```

## 깊이 들어가기

문자열 capitalizing은 일반적으로 문자열을 다루는 프로그래밍에서 자주 사용됩니다. Elixir에서는 String 모듈을 통해 간편하게 이 기능을 사용할 수 있습니다. 이 모듈에는 capitalize와 capitalize_words 함수 외에도 다양한 문자열을 다루는 함수들이 있으므로 필요한 경우 참고하시기 바랍니다.

## 참고

- [Elixir String documentation](https://hexdocs.pm/elixir/String.html)
- [Elixir String capitalize 함수 소스 코드](https://github.com/elixir-lang/elixir/blob/v1.9.0/lib/elixir/lib/string.ex#L2162)
- [Elixir String capitalize_words 함수 소스 코드](https://github.com/elixir-lang/elixir/blob/v1.9.0/lib/elixir/lib/string.ex#L2177)