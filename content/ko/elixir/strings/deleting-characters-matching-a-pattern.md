---
date: 2024-01-20 17:42:11.425280-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) Elixir\uC5D0\uC11C\
  \ \uBB38\uC790\uC5F4 \uB0B4 \uD328\uD134\uC5D0 \uB9DE\uB294 \uBB38\uC790\uB97C \uC0AD\
  \uC81C\uD558\uB294 \uBC29\uBC95\uC740 `String.replace/3` \uD568\uC218\uB97C \uC0AC\
  \uC6A9\uD558\uB294 \uAC83\uC785\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.535267-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) Elixir\uC5D0\uC11C \uBB38\uC790\
  \uC5F4 \uB0B4 \uD328\uD134\uC5D0 \uB9DE\uB294 \uBB38\uC790\uB97C \uC0AD\uC81C\uD558\
  \uB294 \uBC29\uBC95\uC740 `String.replace/3` \uD568\uC218\uB97C \uC0AC\uC6A9\uD558\
  \uB294 \uAC83\uC785\uB2C8\uB2E4."
title: "\uD328\uD134\uC5D0 \uC77C\uCE58\uD558\uB294 \uBB38\uC790 \uC0AD\uC81C"
weight: 5
---

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
