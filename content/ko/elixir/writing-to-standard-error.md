---
title:    "Elixir: 표준 오류에 쓰는 것"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

Erlang 가상 머신의 언어인 Elixir는 함수형 프로그래밍 언어로, 현대적인 웹 개발에서 인기 있는 언어입니다. Elixir는 많은 기능을 제공하지만 중요한 한 가지 기능은 표준 오류로의 쓰기입니다. 이 기능의 이점을 알아보기 전에 가상 머신의 디버깅 체계를 이해하는 것이 중요합니다.

## 왜

코드를 작성하다가 에러가 발생한다면, 그 에러 메시지를 콘솔에서 볼 수 있을 것입니다. 하지만 어떤 경우에는 이 에러를 파일에 기록하는 것이 더 유용한 경우도 있습니다. 또는 프로그램이 종료되기 전에 일부 정보를 콘솔에 출력해야 할 필요가 있을 수 있습니다. 이런 경우에는 표준 오류로의 쓰기가 도움이 됩니다.

## 사용 방법

우선, Elixir의 **IO** 모듈에서 **stderr** 함수를 불러올 수 있습니다. 이 함수에 쓰기를 원하는 데이터를 넘겨주면 됩니다. 예를 들어, 다음과 같은 코드를 작성할 수 있습니다.

```Elixir
IO.stderr("This is an error message") 
```

위 코드를 실행하면, 콘솔에 "This is an error message"가 출력되는 것이 아니라, **STDERR** 파일에 기록될 것입니다. 이 파일을 통해 프로그램이 종료되었을 때도 이 메시지를 확인할 수 있습니다. 또한, 이 함수를 사용하면 특정 파일을 지정하여 에러 메시지를 기록할 수도 있습니다.

더 나은 디버깅을 위해, Elixir에서는 **raise** 키워드를 사용하여 예외를 발생시킬 수 있습니다. 이 예외는 프로그램을 중단시키지 않고 콘솔에 출력되고, 표준 오류 파일에도 기록될 것입니다. 예를 들어,

```Elixir
raise "This is an exception"
```

위 코드를 실행하면, "This is an exception"이 출력되는 것이 아니라, 예외가 발생했음을 나타내는 메시지가 표준 오류 파일에 기록될 것입니다.

더 나은 디버깅을 위해, Elixir에서는 예외를 발생시키는 대신 **Logger** 모듈에서 **error** 함수를 사용하여 로그를 남길 수도 있습니다.

```Elixir
Logger.error("This is a log message")
```

위 코드를 실행하면, 콘솔에 로그 메시지가 출력되지 않고, 대신 **stderr.log** 파일에 기록될 것입니다.

## 깊이 파고들기

표준 오류로의 쓰기는 디버깅에 유용하기만 한 것은 아닙니다. 프로그램을 실행하는 동안 중요한 정보를 콘솔에 보여주려는 경우에도 사용될 수 있습니다. 예를 들어, **HTTPoison** 모듈과 함께 **stderr** 함수를 사용하여 웹 요청을 디버깅할 수 있습니다.

더 나은 프로그램 로깅을 위해, Elixir에서는 강력한 로그 라이브러리인 **Elixir에 logfmt**를 사용할 수 있습니다.

## See Also

- [Elixir의 IO 모듈](https://hexdocs.pm/elixir/IO.html)
- [Elixir의 Logger 모듈](https://hexdocs.pm/logger/Logger.html)
- [HTTPoison과 함께 디버깅하기](https://hexdocs.pm/httpoison/readme.html#debugging-with-io-puts)