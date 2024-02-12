---
title:                "코드를 함수로 구성하기"
aliases: - /ko/elixir/organizing-code-into-functions.md
date:                  2024-01-26T01:10:18.734399-07:00
model:                 gpt-4-1106-preview
simple_title:         "코드를 함수로 구성하기"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## 무엇과 왜?
코드를 함수로 구성하는 것은 관련 연산들을 재사용 가능한 블록으로 청크화하는 것을 의미합니다. 우리는 가독성과 유지보수성을 높이고, 중복을 줄이며, 테스트를 간소화하기 위해 이렇게 합니다.

## 방법:
간단한 엘릭서 함수를 만들어 단어들을 대문자로 시작하게 만들어 봅시다:

```elixir
defmodule StringUtils do
  def capitalize_words(sentence) do
    sentence
    |> String.split()
    |> Enum.map(&String.capitalize/1)
    |> Enum.join(" ")
  end
end

IO.puts StringUtils.capitalize_words("hello elixir world")
```
출력:
```
Hello Elixir World
```
여기에서, 우리는 단어의 대문자화 로직을 `capitalize_words`라는 함수로 깔끔하게 패키징했습니다.

## 심층 분석
엘릭서에서, 그리고 더 넓은 Erlang VM 생태계에서 함수는 일급 시민으로, 문제를 더 작고 관리 가능하며 독립적인 조각으로 쪼개는 철학을 이어받습니다. 역사적으로, 이런 함수적 접근은 람다 계산법과 Lisp에 뿌리를 두고 있으며, 코드를 데이터로 간주하는 철학을 촉진시키고 있습니다.

코드를 구성하는 다른 방법으로는 반복적이거나 동시성 작업을 위해 엘릭서에서 매크로나 프로세스를 사용할 수 있습니다. 구현 측면에서, 엘릭서 함수는 패턴 매칭을 처리할 수 있고 서로 다른 인자(arity)를 받을 수 있어 다재다능함을 부여받습니다.

## 참조
- [엘릭서 공식 문서의 함수에 대해](https://hexdocs.pm/elixir/Kernel.html#functions)
- [Dave Thomas의 "Programming Elixir"](https://pragprog.com/titles/elixir16/programming-elixir-1-6/)
