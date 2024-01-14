---
title:                "Elixir: 새로운 프로젝트 시작하기"
simple_title:         "새로운 프로젝트 시작하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 왜
새로운 프로젝트를 시작하는 이유는 다양합니다. 누군가는 새로운 아이디어를 구현하기 위해 시작할 수 있고, 다른 사람은 성장하는 동적인 언어로서의 엘릭서를 배우기 위해 시작할 수 있습니다. 어떤 이유든 간에, 새로운 프로젝트를 시작하면 새로운 도전과 학습의 기회가 있습니다.

## 어떻게
새로운 프로젝트를 시작하기 전에, 반드시 엘릭서를 설치해야 합니다. 다음은 Windows, Mac 및 Linux에서 엘릭서를 설치하는 방법입니다.
```Elixir
# Windows
choco install elixir

# Mac
brew install elixir

# Linux
sudo apt-get install elixir
```

엘릭서를 설치한 후에는 `mix new 프로젝트이름` 명령어로 새로운 프로젝트를 생성할 수 있습니다. 그리고 `cd 프로젝트이름` 명령어로 해당 프로젝트 디렉토리로 이동한 다음 `mix test`명령어로 프로젝트의 테스트를 실행할 수 있습니다. 이제 새로운 프로젝트를 시작하고 코드를 작성하고 실행할 수 있습니다.

```Elixir
defmodule Hello do
  def greet(name) do
    IO.puts "Hello, #{name}!"
  end
end

Hello.greet("World")

# output: Hello, World!
```

## 깊게 들어가기
새로운 엘릭서 프로젝트를 시작하는 것은 역동적이고 재미있는 경험입니다. 이 언어는 함수형 언어로서 강력한 패턴 매칭 기능을 갖고 있으며, 불변성을 강조하여 안정적인 코드를 작성할 수 있습니다. 또한 멀티 프로세싱을 지원하여 높은 가용성을 유지할 수 있습니다.

새로운 프로젝트를 시작할 때는 최대한 코드를 단순하고 간결하게 작성하는 것이 좋습니다. 또한 테스트 코드를 작성하여 코드의 안정성을 보장하는 것이 중요합니다. 엘릭서에서는 ExUnit이라는 내장된 테스트 라이브러리를 제공하여 효율적으로 테스트할 수 있습니다.

## 이어서 보기
프로그래밍 언어를 배우고 싶은데 엘릭서가 아니라면 다른 프로그래밍 언어를 배울 수 있는 방법을 알아보세요. 아래는 추천하는 자료들입니다.

- [Elixir 공식 홈페이지](https://elixir-lang.org/)
- [Elixir 스타일 가이드](https://github.com/christopheradams/elixir_style_guide)
- [Elixir in Action 책](https://www.manning.com/books/elixir-in-action)
- [Elixir 포럼](https://elixirforum.com/)

참고: 이번 글은 [Elixir 공식 문서](https://elixir-lang.org/getting-started/introduction.html)를 바탕으로 작성되었습니다.