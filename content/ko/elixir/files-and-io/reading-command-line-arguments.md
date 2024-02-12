---
title:                "명령줄 인수 읽기"
aliases:
- /ko/elixir/reading-command-line-arguments/
date:                  2024-01-20T17:55:59.138032-07:00
model:                 gpt-4-1106-preview
simple_title:         "명령줄 인수 읽기"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
명령 줄 인수 읽기는 프로그램이 시작할 때 사용자로부터 입력 데이터를 받는 방법입니다. 이 기능을 사용해서 사용자가 프로그램에게 동작을 지시하거나 설정을 전달할 수 있어요.

## How to:
Elixir에서 명령 줄 인수를 다뤄보겠습니다. 간단하게 System.argv를 사용하세요.

```elixir
defmodule CliDemo do
  def main(args) do
    IO.inspect(args)
  end
end

# 커맨드 라인에서 이렇게 호출합니다:
# elixir cli_demo.exs arg1 arg2 arg3

# 출력 결과:
# ["arg1", "arg2", "arg3"]
```
main 함수는 시작점으로, 명령 줄 인수를 리스트로 받습니다. 이 리스트를 이용하여 필요한 작업을 수행하면 됩니다.

## Deep Dive
Elixir는 Erlang VM 위에서 구동되므로, 명령 줄 인수를 다루는 방법은 Erlang과 유사합니다. 시스템 명령어와 함께 시작하는 데 사용하는 스크립트 형태인 Elixir 스크립트(.exs 파일)에서도 명령 줄 인수를 읽을 수 있습니다. 

커맨드 라인 인수를 파싱할 때, OptionParser 모듈을 사용하는 것도 하나의 방법입니다. 이 모듈은 인수를 키워드 리스트나 맵으로 변환할 수 있게 도와줍니다.
```elixir
parsed_args = OptionParser.parse(args, switches: [option: :string])
```
이는 사용자가 내부 옵션을 보다 명확하게 구분할 수 있게 해줍니다.

## See Also
- [Elixir 공식 문서의 OptionParser](https://hexdocs.pm/elixir/OptionParser.html)
- [Erlang 문서에 나와 있는 저수준 시스템 인터페이스](http://erlang.org/doc/man/init.html)

Elixir의 세계에 오신 것을 환영합니다. 창의력을 발휘하며 자유롭게 코드를 작성해보세요!
