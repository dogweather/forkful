---
date: 2024-01-26 03:48:36.682300-07:00
description: "Elixir\uC5D0\uC11C \uB514\uBC84\uAC70\uB97C \uC0AC\uC6A9\uD55C\uB2E4\
  \uB294 \uAC83\uC740 \uCF54\uB4DC\uB97C \uB2E8\uACC4\uBCC4\uB85C \uC2E4\uD589\uD558\
  \uBA70 \uBCC0\uC218\uB97C \uC870\uC0AC\uD558\uACE0 \uD750\uB984\uC744 \uCD94\uC801\
  \uD558\uC5EC \uBC84\uADF8\uB97C \uC81C\uAC70\uD558\uB294 \uACFC\uC815\uC744 \uC758\
  \uBBF8\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC608\uC0C1\
  \uCE58 \uBABB\uD55C \uC0C1\uD669\uC744 \uC774\uD574\uD558\uACE0 \uC560\uD50C\uB9AC\
  \uCF00\uC774\uC158\uC774 \uC124\uACC4\uB300\uB85C \uB3D9\uC791\uD558\uB3C4\uB85D\
  \ \uD558\uAE30 \uC704\uD574 \uC774 \uC791\uC5C5\uC744 \uC218\uD589\uD569\uB2C8\uB2E4\
  ."
lastmod: '2024-03-13T22:44:54.728325-06:00'
model: gpt-4-0125-preview
summary: "Elixir\uC5D0\uC11C \uB514\uBC84\uAC70\uB97C \uC0AC\uC6A9\uD55C\uB2E4\uB294\
  \ \uAC83\uC740 \uCF54\uB4DC\uB97C \uB2E8\uACC4\uBCC4\uB85C \uC2E4\uD589\uD558\uBA70\
  \ \uBCC0\uC218\uB97C \uC870\uC0AC\uD558\uACE0 \uD750\uB984\uC744 \uCD94\uC801\uD558\
  \uC5EC \uBC84\uADF8\uB97C \uC81C\uAC70\uD558\uB294 \uACFC\uC815\uC744 \uC758\uBBF8\
  \uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC608\uC0C1\uCE58\
  \ \uBABB\uD55C \uC0C1\uD669\uC744 \uC774\uD574\uD558\uACE0 \uC560\uD50C\uB9AC\uCF00\
  \uC774\uC158\uC774 \uC124\uACC4\uB300\uB85C \uB3D9\uC791\uD558\uB3C4\uB85D \uD558\
  \uAE30 \uC704\uD574 \uC774 \uC791\uC5C5\uC744 \uC218\uD589\uD569\uB2C8\uB2E4."
title: "\uB514\uBC84\uAC70 \uC0AC\uC6A9\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
Elixir에서 디버거를 사용한다는 것은 코드를 단계별로 실행하며 변수를 조사하고 흐름을 추적하여 버그를 제거하는 과정을 의미합니다. 프로그래머들은 예상치 못한 상황을 이해하고 애플리케이션이 설계대로 동작하도록 하기 위해 이 작업을 수행합니다.

## 사용 방법:
Elixir는 `:debugger`라는 내장 그래픽 디버거를 제공합니다. 이를 사용하려면 실행 중인 프로세스에 연결하여 시작해야 합니다.

먼저, `iex` 세션 내에서 `:debugger`가 시작되었는지 확인하세요:
```elixir
iex> :debugger.start()
{:ok, #PID<0.108.0>}
```

이제, 디버그하고 싶은 코드 모듈을 해석하세요:
```elixir
iex> :int.ni(MyApp.MyModule)
{:module, MyApp.MyModule}
```

중단점을 설정할 수 있습니다:
```elixir
iex> :int.break(MyApp.MyModule, line_number)
:ok
```

그 다음, 함수를 실행하여 중단점에서 코드를 단계별로 실행하세요:
```elixir
iex> MyApp.MyModule.my_function(arg1, arg2)
# 디버거는 중단점이 있는 줄에서 실행을 일시 중지합니다
```

## 심층 탐구
Elixir의 `:debugger` 이전에는, Erlang이 Elixir에서 사용하는 디버거를 제공했습니다; 이는 동시성 프로세스를 처리하는 데 매우 강력하며, Erlang VM(BEAM)의 장점입니다. 일부 다른 디버거와 달리, `:debugger`는 Elixir에서 데이터의 불변성 때문에 실행 중 변수를 수정할 수 없습니다. 대안으로, 코드의 어느 지점에서든 실행을 일시 중지하고 REPL로 점프할 수 있는 `IEx.pry`가 있어 매우 편리할 수 있습니다.

`:debugger`는 그래픽 인터페이스에 좋지만, 코드를 단계별로 실행하는 것을 특별히 목표로 하지 않지만 프로세스 검사와 시스템 메트릭스를 제공하는 내장 `:observer` 도구를 선호하는 사람들도 있습니다. Elixir 커뮤니티도 `visualixir`와 `rexbug` 같은 도구를 제공하여 기본 도구를 넘어 디버그 도구 생태계를 확장합니다.

## 참고
- 공식 Elixir 시작 가이드의 디버깅: https://elixir-lang.org/getting-started/debugging.html
- Erlang의 `:debugger` 문서: http://erlang.org/doc/apps/debugger/debugger_chapter.html
- 디버깅 기법에 대한 Elixir 포럼 토론: https://elixirforum.com/c/elixir-questions/elixir-questions-questions-help/15
