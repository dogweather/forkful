---
title:    "Elixir: 컴퓨터 프로그래밍에서의 명령줄 인수 읽기"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

**Elixir 프로그래밍을 위한 명령 줄 인자 읽는 방법**

## 왜?

언어를 배우기 위해서는 항상 그 언어로 무엇을 할 수 있는지 궁금할 것입니다. Elixir 또한 이와 같지 않습니다. Elixir는 생성적으로 방대한 언어를 잘 다룰 수 있도록 해주는 뛰어난 툴링이 있습니다. 이러한 툴링 중에서, 명렁 줄 인자(argument)를 다루는 것은 매우 간단하지만 유용한 기술입니다. 이를 이해하는 데 도움이 될수도 있고, 알아두어도 좋습니다.

## 사용 방법

명령 줄 인자를 Elixir로 읽는 것은 아주 쉽습니다. 다음과 같은 샘플 코드를 살펴보세요.

```Elixir
args = System.argv
IO.inspect args
```

위 코드를 실행하면, `Elixir elixir_file.exs hello world`를 인자로 받았을 경우 `["hello", "world"]`를 출력하게 됩니다. 이제 이 읽힌 인자들을 가지고 원하는 대로 처리할 수 있겠죠?

## 더 깊게

위에서 살펴본 예제는 단순히 명령 줄에서 인자를 읽는 방법을 보여주는 것이었습니다. 이제 더 깊게 파고들어, 들어오는 인자의 유형을 다양하게 처리할 수 있는 방법을 살펴보겠습니다. 예를 들어, 아래와 같은 경우를 생각해봅시다.

```Elixir
Elixir elixir_file.exs hello --name "John Doe"
```

우리는 위의 예시에서 `--name`이라는 인자를 추가로 받습니다. 이를 처리하기 위해서는 `OptionParser` 모듈을 사용하면 됩니다. 다음과 같이 예제 코드를 작성해보세요.

```Elixir
args = System.argv
opts = OptionParser.parse(args, switches: [name: :string])
```

위의 코드를 실행하면 우리는 `opts`라는 변수를 통해 `["hello"]` 와 `%{name: "John Doe"}`를 얻을 수 있습니다.

## 관련 자료

- Elixir 공식 문서: [명령 줄 인수 처리하기](https://hexdocs.pm/elixir/OptionParser.html)
- Elixir School: [명령 줄 인수 다루기](https://elixirschool.com/lessons/basics/command-line-args/)
- Programming Phoenix: [명령 줄에서 제어하려는 띠용한 것들](https://pragprog.com/book/phoenix/programming-phoenix)

## 더 알아보기

이제 여러분은 Elixir로 명령 줄 인자를 읽는 방법을 잘 이해하게 되었습니다. 하지만 Elixir는 더 많은 가치가 있는 언어입니다. 더 많은 정보를 알고 싶다면 아래의 자료들을 참고하세요.

- Elixir 공식 홈페이지: [https://elixir-lang.org/](https://elixir-lang.org/)
- Elixir 한국 사용자 그룹: [https://www.facebook.com/groups/elixirkr/](https://www.facebook.com/groups/elixirkr/)
- Elixir 공식 포럼: [https://elixirforum.com/](https://elixirforum.com/)