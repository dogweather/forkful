---
date: 2024-01-20 17:55:59.138032-07:00
description: "\uBA85\uB839 \uC904 \uC778\uC218 \uC77D\uAE30\uB294 \uD504\uB85C\uADF8\
  \uB7A8\uC774 \uC2DC\uC791\uD560 \uB54C \uC0AC\uC6A9\uC790\uB85C\uBD80\uD130 \uC785\
  \uB825 \uB370\uC774\uD130\uB97C \uBC1B\uB294 \uBC29\uBC95\uC785\uB2C8\uB2E4. \uC774\
  \ \uAE30\uB2A5\uC744 \uC0AC\uC6A9\uD574\uC11C \uC0AC\uC6A9\uC790\uAC00 \uD504\uB85C\
  \uADF8\uB7A8\uC5D0\uAC8C \uB3D9\uC791\uC744 \uC9C0\uC2DC\uD558\uAC70\uB098 \uC124\
  \uC815\uC744 \uC804\uB2EC\uD560 \uC218 \uC788\uC5B4\uC694."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:54.744457-06:00'
model: gpt-4-1106-preview
summary: "\uBA85\uB839 \uC904 \uC778\uC218 \uC77D\uAE30\uB294 \uD504\uB85C\uADF8\uB7A8\
  \uC774 \uC2DC\uC791\uD560 \uB54C \uC0AC\uC6A9\uC790\uB85C\uBD80\uD130 \uC785\uB825\
  \ \uB370\uC774\uD130\uB97C \uBC1B\uB294 \uBC29\uBC95\uC785\uB2C8\uB2E4."
title: "\uBA85\uB839\uC904 \uC778\uC218 \uC77D\uAE30"
weight: 23
---

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
