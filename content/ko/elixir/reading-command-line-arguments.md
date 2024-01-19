---
title:                "명령줄 인수 읽기"
html_title:           "Arduino: 명령줄 인수 읽기"
simple_title:         "명령줄 인수 읽기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 무엇과 왜?

커맨드 라인 인수 읽기는 프로그램이 사용자 입력을 명령 줄 -즉, 프로그램 시작 시마다 -로부터 받는 방법입니다. 프로그래머는 이것을 통해 프로그램의 동작을 사용자 정의할 수 있습니다.

## 어떻게 사용하나요?

Elixir는 커맨드 라인 인수에 접근하기 위해 System.argv을 사용합니다. 이를 사용하여 사용자가 제공하는 옵션에 따라 프로그램을 세부 조정할 수 있습니다. 

```Elixir
defmodule Hello do
  def greet do
    [name | _] = System.argv()
    IO.puts "Hello, #{name}!"
  end
end

Hello.greet()
```
실행 시 이름을 인수로서 제공합니다:

```
$ elixir hello.exs World
Hello, World!
```

## 깊게 알아보기

커맨드 라인 인수는 early Unix shell 프로그래밍부터 오랜 역사를 가지고 있습니다. 이는 사용자에게 프로그램을 확장하고 세부 코드를 작성할 수 있게 하여 초기 대화형 프로그래밍 환경과 비교하여 더 많은 유연성을 제공했습니다.

Elixir의 `System.argv/0` 함수 외에도 Erlang/Elixir 생태계에서는 `:init.get_plain_arguments/0`와 같은 함수를 사용하여 매개변수를 처리하는 방법도 있습니다. 후자 함수는 OTP 애플리케이션에 더욱 일반적입니다.

내부적으로, Elixir는 Erlang의 `:init` 모듈을 wrapper하여 이 기능을 제공합니다. `:init.get_plain_arguments/0` 함수 역시 이 모듈에 정의되어 있습니다.

## 참고 자료

생태계 내 다른 Erlang/Elixir 라이브러리도 명령 줄 인수 처리를 지원합니다:
1. [Elixir OptionParser](https://hexdocs.pm/elixir/OptionParser.html)
2. [Erlang getopt](https://github.com/jcomellas/getopt)