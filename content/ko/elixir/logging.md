---
title:                "로깅"
date:                  2024-01-26T01:01:58.228262-07:00
model:                 gpt-4-1106-preview
simple_title:         "로깅"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/logging.md"
---

{{< edit_this_page >}}

## 무엇인가 & 왜 사용하는가?
소프트웨어 개발에서 로깅은 주로 파일이나 외부 시스템에 프로그램이 실행 중일 때 발생하는 이벤트들을 기록하는 기술입니다. 개발자들은 소프트웨어의 행동을 이해하고 문제를 해결하며 운영 기록을 유지하기 위해 이를 수행합니다. 이는 디버깅과 애플리케이션의 상태를 모니터링하는 데 필수적입니다.

## 사용 방법:
Elixir에서 정보를 기록하는 주요 방법은 내장된 `Logger` 모듈을 통하는 것입니다. 어떻게 사용하는지 여기에 예시가 있습니다:

```elixir
defmodule MyApplication do
  require Logger

  def do_something_important(param) do
    Logger.info("Starting important process with param: #{param}")

    # 작업이 수행되는 것을 시뮬레이션
    :timer.sleep(1000)

    Logger.debug("Process completed.")
  rescue
    error -> Logger.error("An error occurred: #{inspect(error)}")
  end
end

# 로그를 보려면 함수를 호출하기만 하면 됩니다:
MyApplication.do_something_important("MyParam")
```

이 간단한 코드 조각은 다른 레벨(`info`, `debug`, `error`)에서 로깅하는 방법을 보여줍니다. 이를 실행하면 Logger 레벨을 `:debug`로 설정하지 않는 한 디버그 메시지는 보이지 않을 것입니다. 기본적으로 Elixir의 Logger는 `:info` 레벨보다 낮은 로그 메시지를 필터링합니다.

`:info` 레벨에서의 샘플 출력은 다음과 같아 보일 것입니다:
```
14:32:40.123 [info]  Starting important process with param: MyParam
14:32:41.126 [error] An error occurred: %RuntimeError{message: "runtime error"}
```

## 깊이 있게 알아보기:
Elixir의 `Logger`는 언어의 초창기부터 내장된 도구로, Erlang 같은 다른 BEAM 언어들의 로깅 시스템에 영향을 받았습니다. Logger는 다양한 로깅 레벨을 제공합니다 – `:debug`, `:info`, `:warn`, `:error` – 그리고 전환 가능하며, 로그 메시지를 처리하기 위해 다른 백엔드를 연결할 수 있습니다.

내장 Logger보다 복잡한 상황에서 사용할 수 있는 대안으로, `Logstash` 나 Elixir용 `Sentry`와 같은 로깅 라이브러리를 사용하는 것입니다. 이러한 라이브러리들은 오류 추적 및 보다 시각적 형식의 집계와 같은 추가적인 기능을 제공할 수 있습니다. 로컬 개발을 위해 Elixir 개발자들은 종종 그 간단함과 BEAM VM과의 통합 때문에 내장 Logger 기능에 의존합니다.

내부적으로 Logger 모듈은 비동기 및 동기 로깅을 제공합니다. 기본값인 비동기 로깅은 애플리케이션의 실행을 로깅 메시지로 인해 차단하지 않습니다. 이는 로깅이 성능에 부정적인 영향을 미치지 않도록 보장합니다. 그러나 메시지가 전송된 순서대로 로그가 기록되는 것을 보장해야 하는 경우를 위해 동기 로깅을 활성화 할 수 있습니다.

Logger 설정은 Elixir 애플리케이션의 `config/config.exs` 파일에서 조정할 수 있으며, 여기서 로깅 레벨, 형식, 메타데이터 등을 설정할 수 있습니다. 항상 다양한 환경에 대한 로깅 레벨과 출력을 조정하는 것을 잊지 마십시오; 생산 시스템에 장황한 디버그 로그가 범람하는 것을 원하지 않을 것입니다.

## 또한 보기:
- 공식 Elixir Logger 문서: https://hexdocs.pm/logger/Logger.html
- Elixir 로깅 모범 사례에 관한 블로그 포스트: https://blog.appsignal.com/2020/05/06/elixir-logging-tips-and-tricks.html
- Hex에서의 Elixir용 Sentry: https://hex.pm/packages/sentry
- Logger에 대한 Elixir School의 수업: https://elixirschool.com/en/lessons/specifics/debugging/#logging
