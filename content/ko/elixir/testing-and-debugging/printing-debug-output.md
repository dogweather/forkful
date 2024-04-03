---
date: 2024-01-20 17:52:33.657567-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) Elixir\uC5D0\uC11C\uB294\
  \ `IO.inspect/2` \uAE30\uB2A5\uC744 \uC0AC\uC6A9\uD574\uC11C \uBCC0\uC218\uC758\
  \ \uAC12\uC744 \uCD9C\uB825\uD558\uACE0 \uBD84\uC11D\uD560 \uC218 \uC788\uC2B5\uB2C8\
  \uB2E4. \uB2E4\uC74C \uC608\uC81C\uB97C \uD655\uC778\uD574 \uBCF4\uC138\uC694."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:54.725314-06:00'
model: gpt-4-1106-preview
summary: "Elixir\uC5D0\uC11C\uB294 `IO.inspect/2` \uAE30\uB2A5\uC744 \uC0AC\uC6A9\uD574\
  \uC11C \uBCC0\uC218\uC758 \uAC12\uC744 \uCD9C\uB825\uD558\uACE0 \uBD84\uC11D\uD560\
  \ \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uB514\uBC84\uADF8 \uCD9C\uB825\uC744 \uCC0D\uC5B4\uBCF4\uAE30"
weight: 33
---

## How to: (어떻게 하나요?)
Elixir에서는 `IO.inspect/2` 기능을 사용해서 변수의 값을 출력하고 분석할 수 있습니다. 다음 예제를 확인해 보세요:

```elixir
# 값을 출력하면서 변수에 바인딩
my_list = IO.inspect [1, 2, 3]
# 출력: [1, 2, 3]

# 옵션을 이용해 라벨 추가
IO.inspect my_list, label: "Debugging my_list"
# 출력: Debugging my_list: [1, 2, 3]

# 함수의 사이드 이펙트를 확인하기 위해 파이프라인에 사용
[1, 2, 3]
|> Enum.map(&(&1 * 2))
|> IO.inspect(label: "After map")
|> Enum.sum()
# 출력: After map: [2, 4, 6]
```

IO.puts/1을 이용해서 간단한 메시지를 출력할 수도 있어요:

```elixir
IO.puts "Hello, world!"
# 출력: Hello, world!
```

## Deep Dive (심화 정보)
디버깅은 소프트웨어 개발에 필수적입니다. Elixir에서 `IO.inspect/2`는 개발 중 버그를 찾거나 데이터 흐름을 파악하는 데 자주 쓰입니다. Elixir 이전의 언어들도 비슷한 도구를 가지고 있으며 `print`, `console.log`, 혹은 `p` 같은 함수를 사용했습니다.

`IO.inspect/2`는 다양한 옵션을 가지고 있어 데이터를 다른 형태로 나타내거나 더 상세한 정보를 출력할 수 있습니다. 다만, 이를 남발하면 오히려 코드를 읽기 어려워지니 주의하세요.

다른 도구들, 예를 들어 `:debugger` 모듈이나 Erlang's Observer 같은 GUI 기반 도구로 더 깊은 디버깅이 가능합니다. 이들은 더 복잡한 시스템을 다룰 때 유용하나, 셋업이 복잡하거나 배우기가 어려울 수 있습니다.

## See Also (더 보기)
- Elixir 공식 문서의 `IO` 모듈: [https://hexdocs.pm/elixir/IO.html](https://hexdocs.pm/elixir/IO.html)
- Elixir School의 디버깅 강좌: [https://elixirschool.com/en/lessons/specifics/debugging/](https://elixirschool.com/en/lessons/specifics/debugging/)
- Elixir Forum에서의 디버깅 팁: [https://elixirforum.com/t/debugging-tips-and-tricks/15325](https://elixirforum.com/t/debugging-tips-and-tricks/15325)
