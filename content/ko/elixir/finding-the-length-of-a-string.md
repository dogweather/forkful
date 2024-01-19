---
title:                "문자열의 길이 찾기"
html_title:           "Lua: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열의 길이를 찾는 것은 문자에 몇 개가 있는지 사용자가 확인하는 방법입니다. 프로그래머는 이를 이용해 빈 문자열을 체크하거나 특정 길이의 문자열을 처리하는 등의 작업을 수행합니다.

## 어떻게 ?

Elixir에서 문자열의 길이를 찾으려면 `String.length/1` 함수를 사용합니다. 기본적인 예제를 살펴봅시다.

```elixir
IO.puts String.length("안녕하세요")
```

위 코드를 실행하면, 출력 결과로 5가 나옵니다.

```elixir
IO.puts String.length("")
```

위 코드를 실행하면, 출력 결과로 0이 나옵니다.

## Deep Dive

Elixir의 `String.length/1` 기능은 Unicode 문자열을 정확하게 측정하기 위한 것입니다. Elixir의 초기 버전부터 이 기능이 포함되어 있습니다.

대안으로는 수동으로 문자열을 루프하여 카운트를 증가시킬 수 있지만, 이는 더 복잡하고 느립니다. 또한, 이 방법은 문자열에 유니코드 문자가 포함된 경우 정확한 결과를 얻지 못할 수 있습니다.

`String.length/1` 함수는 각 문자의 바이트 길이를 측정하고 더하는 방식으로 구현되어 있습니다.

## See Also

다음은 이 주제와 관련된 몇가지 유용한 링크입니다:

- Elixir 공식 문서: [Elixir String](https://hexdocs.pm/elixir/String.html)
- Stack Overflow: [How to get string length in Elixir](https://stackoverflow.com/questions/43004961/how-to-get-string-length-in-elixir)
- Elixir School: [String functions](https://elixirschool.com/en/lessons/basics/strings)