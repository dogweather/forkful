---
title:                "패턴에 일치하는 문자 삭제"
date:                  2024-01-20T17:42:11.425280-07:00
model:                 gpt-4-1106-preview
simple_title:         "패턴에 일치하는 문자 삭제"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
패턴에 맞는 문자 삭제는 문자열에서 특정 패턴이나 문자를 찾아서 제거하는 것을 말합니다. 프로그래머들은 데이터를 정제하거나, 입력값에서 불필요한 부분을 제거하기 위해 이 작업을 자주 수행합니다.

## How to: (어떻게 하나요?)
Elixir에서 문자열 내 패턴에 맞는 문자를 삭제하는 방법은 `String.replace/3` 함수를 사용하는 것입니다. 

```elixir
# 영문자를 제거합니다.
string = "안녕하세요! Hello123"
result = String.replace(string, ~r/[a-zA-Z]/, "")
IO.puts(result)  # "안녕하세요! 123"

# 숫자를 제거합니다.
string = "2023년, Elixir를 배우자!"
result = String.replace(string, ~r/\d/, "")
IO.puts(result)  # "년, Elixir를 배우자!"

# 특정 단어를 제거합니다.
string = "Elixir는 매력적인 프로그래밍 언어입니다."
result = String.replace(string, "매력적인 ", "")
IO.puts(result)  # "Elixir는 프로그래밍 언어입니다."
```

## Deep Dive (깊이 있게 알아보기)
Elixir에서 문자열 처리는 매우 유연합니다. `String.replace/3` 뿐만 아니라, 정규 표현식을 활용해 다양한 패턴 매칭을 할 수 있습니다. Elixir가 Erlang VM 위에서 동작하기 때문에, 이 문자열 처리 능력은 Erlang의 강력한 문자열 처리 기능에서 유래합니다. 

대안으로, 때에 따라 `String.slice/3`나 `String.trim/2` 등 다른 문자열 처리 함수들을 사용할 수도 있지만, 정규 표현식을 이용한 제거는 매우 구체적인 사용 사례들에 필요합니다. 성능 측면에서, Elixir의 문자열 처리는 내부적으로 Binary 데이터로 취급되므로 매우 효율적입니다. 그러나 복잡한 패턴을 자주 사용하면 성능 이슈가 생길 수 있으므로, 필요한 경우에만 정규 표현식을 사용하는 것이 좋습니다.

## See Also (더 알아보기)
- [Elixir String Module Documentation](https://hexdocs.pm/elixir/String.html)
- [Learn Regular Expressions](https://regexone.com/)
- [Elixir School on Strings](https://elixirschool.com/en/lessons/basics/strings/)
- [Erlang Efficiency Guide](https://www.erlang.org/doc/efficiency_guide/myths.html)
