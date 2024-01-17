---
title:                "새 프로젝트 시작하기"
html_title:           "Elixir: 새 프로젝트 시작하기"
simple_title:         "새 프로젝트 시작하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
새로운 프로젝트를 시작하는 것은 새로운 소프트웨어를 만드는 것을 말합니다. 프로그래머들은 이것을 하게 되는 이유는 자신이 구현하려는 새로운 아이디어를 기반으로 더 나은 소프트웨어를 만들기 위해서입니다.

## 해보자:
```Elixir
defmodule HelloWorld do
  def hello(name) do
    IO.puts "Hello #{name}!"
  end
end

HelloWorld.hello("World")
```
> 출력결과: Hello World!

## 깊은 이해:
- 시작하는 프로젝트는 새로운 코드를 작성하는 것 외에도 기존 코드를 수정하거나 배포하는 것을 포함합니다.
- 다른 대안으로는 이미 정식 출시된 프로젝트를 기반으로 새로운 소프트웨어를 만드는 것도 있습니다.
- 에릭쌍(Ericsson)에서 개발한 언어인 Erlang와 ErlangVM의 영향을 받아 만들어진 Elixir는 함수형 프로그래밍 언어로서 높은 확장성과 안정성을 제공합니다.

## 더 알아보기:
- Elixir 공식 홈페이지 - https://elixir-lang.org/
- Erlang 공식 홈페이지 - https://www.erlang.org/
- ErlangVM 공식 홈페이지 - https://www.erlang.org/faq/what-is-the-erlang-vm