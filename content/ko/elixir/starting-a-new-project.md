---
title:                "새로운 프로젝트 시작하기"
html_title:           "Elixir: 새로운 프로젝트 시작하기"
simple_title:         "새로운 프로젝트 시작하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 왜

새 프로젝트를 시작하는 것이 중요할까요? 여러분들이 Elixir 프로그래밍에 익숙하다면, 당연하다고 생각할 수도 있겠지만, 새로운 프로젝트를 시작하는 것은 거대한 잠재력을 지니고 있습니다. 여러분들의 아이디어를 현실화시킬 수 있고, 새로운 기술과 아이디어를 시도하고 성장할 수 있는 기회가 될 수 있습니다.

## 어떻게 시작할까요

```elixir
defmodule Project do  # 새로운 모듈 선언
  # 모듈 내에서 함수 정의하기
  def hello(name) do
    IO.puts "Hello, #{name}!"
  end

  def goodbye(name) do
    IO.puts "Goodbye, #{name}!"
  end
end
```

위의 예시 코드처럼 새로운 모듈을 선언하여 프로젝트를 시작할 수 있습니다. 모듈 내에서 여러분들의 함수를 정의하고, 필요할 때마다 호출하여 사용할 수 있습니다.

```elixir
Project.hello("Elixir") # 출력: Hello, Elixir!
Project.goodbye("Elixir") # 출력: Goodbye, Elixir!
```

위 코드를 실행하면 새로운 프로젝트를 시작하고, 여러분들의 아이디어를 구현할 수 있습니다. 또한 Elixir의 강력한 패턴 매칭 기능을 사용하여 더욱 유연하고 간결한 코드를 작성할 수도 있습니다.

## 딥 다이브

새로운 프로젝트를 시작할 때, 가장 중요한 것은 생각하는 것을 코드로 옮기는 것입니다. 코드로 옮기지 않으면 아이디어는 단지 생각 속에 머물게 됩니다. Elixir는 함수형 프로그래밍 언어로서 강력한 함수와 패턴 매칭 기능을 제공하여 생각하는 것을 코드로 옮기는 것을 더욱 쉽고 효율적으로 도와줍니다.

또한 Elixir는 높은 확장성과 고성능을 지닌 언어입니다. 이로 인해 새로운 프로젝트에서 생각하는 것을 구체적으로 구현하고, 더 많은 사용자들에게 제공할 수 있게 됩니다.

## 더 알아보기

- [Elixir 공식 홈페이지](https://elixir-lang.org/) 
- [Elixir 스타트 가이드](https://elixir-lang.org/getting-started/introduction.html)
- [Elixir 패턴 매칭 가이드](https://elixir-lang.org/getting-started/pattern-matching.html)

## 더 보기

- [Elixir 문서 번역 프로젝트](https://github.com/elixir-lang/elixir-lang.github.com)