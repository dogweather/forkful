---
title:                "Elixir: 명령 줄 인수 읽기"
simple_title:         "명령 줄 인수 읽기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 왜

커맨드 라인 인자를 읽는 데 왜 관심을 가져야 하는지는 중요한 질문입니다. Elixir는 강력하고 다양한 기능을 제공하므로 이 기능을 활용하는 것이 중요합니다.

## 어떻게

커맨드 라인 인자를 읽는 방법은 간단합니다. 우리는 먼저 ```System.argv()``` 함수를 사용하여 커맨드 라인 인자를 리스트로 가져올 수 있습니다. 이후에는 사용자가 원하는 방식으로 데이터를 처리할 수 있습니다. 아래는 간단한 예시입니다.

```elixir
defmodule CommandLine do
  def read_args do
    args = System.argv()
    IO.inspect(args)
  end
end

CommandLine.read_args()
```

위의 코드를 실행해보면, 커맨드 라인에서 사용한 인자들이 리스트로 출력되는 것을 볼 수 있습니다. 예를 들어, 만약 ```elixir my_file.exs arg1 arg2```와 같이 커맨드 라인에서 실행한다면, ```["arg1", "arg2"]```가 출력될 것입니다.

## 깊이 파고들기

커맨드 라인 인자를 읽는 것은 Elixir의 기본 함수 중 하나입니다. 그렇기 때문에 우리는 다양한 방식으로 이 기능을 활용할 수 있습니다. 예를 들어, 우리는 애플리케이션을 실행할 때 사용자가 입력한 옵션을 커맨드 라인 인자로 받아서 그에 따라 동작하도록 할 수 있습니다. 또한, 커맨드 라인 인자를 통해 사용자가 원하는 파일의 경로를 받아서 해당 파일을 열어서 처리할 수도 있습니다. 이처럼 커맨드 라인 인자는 우리가 애플리케이션을 더 유동적이고 다양한 방식으로 동작하도록 할 수 있게 해준다는 것을 기억해주세요.

# 더 많은 정보

더 많은 정보를 원한다면, [Elixir 공식 문서](https://hexdocs.pm/elixir/System.html#argv/0)를 참고하시기 바랍니다. 여기에는 더 많은 예시 코드와 설명이 제공되어 있으며, 더 자세한 정보를 얻을 수 있습니다.

# 관련 링크

- [Elixir 공식 문서](https://hexdocs.pm/elixir/System.html#argv/0)
- [Elixir 프로그래밍 기초](https://brunch.co.kr/@junho85/14)
- [Elixir 커맨드 라인 인자 다루기](https://medium.com/@sepidehsohrabi/handle-command-line-argument-in-elixir-a1bb540ca21b)