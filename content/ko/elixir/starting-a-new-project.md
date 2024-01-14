---
title:                "Elixir: 새 프로젝트 시작하기"
programming_language: "Elixir"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

Elixir 프로그래밍: 새로운 프로젝트를 시작하는 이유

## 왜 이렇게 하나요?

새로운 프로젝트를 시작하는 것은 여러 가지 이유가 있습니다. 어떤 경우에는 새로운 도전에 도전하고 싶어서, 어떤 경우에는 새로운 기술을 배우고 싶어서, 그리고 어떤 경우에는 새로운 문제를 해결하고 싶어서 시작하는 것이 있습니다. Elixir는 다양한 프로젝트를 시작하기에 적합한 언어입니다. 지금부터 Elixir를 사용해 새로운 프로젝트를 시작하는 방법을 알아보겠습니다.

## 어떻게 하나요?

새로운 프로젝트를 시작하기 전에 먼저 Elixir를 설치해야 합니다. 설치 방법은 OS에 따라 다를 수 있지만, 대부분의 경우에는 간단한 명령어로 설치할 수 있습니다.

```Elixir
$ sudo apt install elixir # Ubuntu
$ brew install elixir # MacOS
```
설치가 완료되면 프로젝트를 시작할 준비가 끝났습니다. 이제 프로젝트 폴더를 만들고 그 안에 Elixir 프로젝트를 만들어보겠습니다. 아래의 예제 코드와 함께 간단한 ToDo 앱을 만들어보겠습니다.

```Elixir
# lib/todo.ex
defmodule Todo do
  def new(), do: ["Buy groceries", "Do laundry", "Clean room"]
end
```

```Elixir
# test/todo_test.exs
defmodule TodoTest do
  use ExUnit.Case

  test "new" do
    assert Todo.new() == ["Buy groceries", "Do laundry", "Clean room"]
  end
end
```

위의 코드를 실행하기 위해 터미널에서 `mix test` 명령어를 입력하면, 결과로 `1 run, 1 failure`가 출력됩니다. 이는 우리가 아직 `add` 함수를 정의하지 않았기 때문입니다. `add` 함수를 정의해서 테스트를 통과시키고, `mix test`를 다시 실행해보세요.

## 깊게 파보기

새로운 프로젝트를 시작할 때 가장 중요한 것은 디자인입니다. 어떤 코드를 작성할지, 어떤 함수를 정의할지, 어떤 변수를 사용할지 등을 정하는 것은 프로젝트의 성공 여부에 큰 영향을 미칩니다. Elixir를 사용하면서 다양한 디자인 패턴들을 배우고 적용해보세요. 그리고 다른 언어와는 다른 함수형 프로그래밍의 장점을 이용해서 더욱 유연하고 간결한 코드를 작성해보세요.

## 보기

- [Elixir 공식 사이트](https://elixir-lang.org/)
- [Elixir 커뮤니티 포럼](https://elixirforum.com/)
- [Elixir School](https://elixirschool.com/ko/)