---
date: 2024-01-20 17:52:33.657567-07:00
description: "\uB514\uBC84\uADF8 \uCD9C\uB825\uC740 \uCF54\uB4DC\uC5D0\uC11C \uBC1C\
  \uC0DD\uD55C \uB370\uC774\uD130\uB098 \uC0C1\uD0DC\uB97C \uD655\uC778\uD558\uAE30\
  \ \uC704\uD574 \uC801\uD569\uD55C \uC694\uC18C\uB97C \uC784\uC2DC\uC801\uC73C\uB85C\
  \ \uD45C\uC2DC\uD558\uB294 \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uBC84\uADF8\uB97C \uCC3E\uACE0, \uD750\uB984\uC744 \uD655\uC778\uD558\
  \uBA70, \uC560\uD50C\uB9AC\uCF00\uC774\uC158\uC758 \uB3D9\uC791\uC744 \uC774\uD574\
  \uD558\uAE30 \uC704\uD574 \uC774\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:54.725314-06:00'
model: gpt-4-1106-preview
summary: "\uB514\uBC84\uADF8 \uCD9C\uB825\uC740 \uCF54\uB4DC\uC5D0\uC11C \uBC1C\uC0DD\
  \uD55C \uB370\uC774\uD130\uB098 \uC0C1\uD0DC\uB97C \uD655\uC778\uD558\uAE30 \uC704\
  \uD574 \uC801\uD569\uD55C \uC694\uC18C\uB97C \uC784\uC2DC\uC801\uC73C\uB85C \uD45C\
  \uC2DC\uD558\uB294 \uAC83\uC785\uB2C8\uB2E4."
title: "\uB514\uBC84\uADF8 \uCD9C\uB825\uC744 \uCC0D\uC5B4\uBCF4\uAE30"
weight: 33
---

## What & Why? (무엇이며 왜?)

디버그 출력은 코드에서 발생한 데이터나 상태를 확인하기 위해 적합한 요소를 임시적으로 표시하는 것입니다. 프로그래머들은 버그를 찾고, 흐름을 확인하며, 애플리케이션의 동작을 이해하기 위해 이를 사용합니다.

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
