---
title:                "명령 줄 인수 읽기"
html_title:           "Elixir: 명령 줄 인수 읽기"
simple_title:         "명령 줄 인수 읽기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

만약 당신이 프로그래밍을 공부하거나, 개발을 하거나, 혹은 이를 통해 새로운 언어를 배우려면, 커맨드 라인 인자를 읽는 방법은 필수적입니다.

## 어떻게 하는지

커맨드 라인 인자를 읽는 것은 간단합니다. 우선, `System.argv` 함수를 사용하여 전달받은 인자들의 리스트를 얻으세요. 그 다음, `Enum.each` 함수를 사용하여 각 인자들에 대한 작업을 수행할 수 있습니다. 아래는 간단한 예제 코드입니다.

```Elixir
defmodule CommandLine do
  def print_args do
    System.argv 
    |> Enum.each(fn(arg) -> IO.puts(arg) end) 
  end
end

CommandLine.print_args()
```

위 코드를 `script.exs`라는 파일로 저장하고, 터미널에서 `elixir script.exs hello world`라는 명령을 실행하면 아래와 같은 결과가 출력됩니다.

```
hello
world
```

간단하죠? 이제 여러분이 원하는대로 커맨드 라인 인자들을 사용할 수 있습니다.

## 더 깊이 들어가기

`System.argv` 함수는 여러 옵션을 제공하지만, 몇 가지 중요한 것들만 살펴보겠습니다. 먼저, `System.argv()`를 호출하는 것만으로도 커맨드 라인 인자들을 리스트로 얻을 수 있습니다.

또한, `System.argv(0)`는 실행되는 스크립트의 경로를 나타내는 문자열을 반환합니다. 실행되는 스크립트의 이름을 알고 싶다면, `Path.basename(System.argv(0))`를 사용하시면 됩니다. 그리고 `System.argv([])`에 대한 문서를 읽어보시면 더 많은 옵션들을 찾을 수 있을 것입니다.

## 자세히 알아보기

커맨드 라인 인자를 읽는 방법은 매우 간단하며, 새로운 언어를 배우기 전에 먼저 익히는 것이 좋습니다. 그리고 더 많이 알아보고 싶다면, [Elixir 문서](https://hexdocs.pm/elixir/System.html)를 참고하세요.

## 관련 링크

- [Elixir 문서](https://hexdocs.pm/elixir/System.html)
- [Enum.each 함수 문서](https://hexdocs.pm/elixir/Enum.html#each/2)
- [IO.puts 함수 문서](https://hexdocs.pm/elixir/IO.html#puts/1)