---
title:                "표준 에러에 쓰기"
aliases: - /ko/elixir/writing-to-standard-error.md
date:                  2024-02-03T19:33:06.372138-07:00
model:                 gpt-4-0125-preview
simple_title:         "표준 에러에 쓰기"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇과 왜?

Elixir에서 표준 오류(stderr)로 쓰기는 오류 메시지와 진단을 주 출력(stdout)과 분리하여 직접하는 방법입니다. 프로그래머는 stderr를 사용하여 프로그램의 주 출력을 혼란스럽게 하지 않고 오류를 디버그하고 처리함으로써 문제를 쉽게 식별하고 해결할 수 있습니다.

## 어떻게:

Elixir에서는 `IO.puts/2` 및 `IO.warn/2`과 같은 `IO` 모듈 함수를 사용하여 표준 오류에 메시지를 작성할 수 있습니다:

```elixir
# stderr에 간단한 메시지를 쓰기
IO.puts(:stderr, "Error: 무언가 잘못되었습니다!")

# 경고/오류에 더 의미 있는 IO.warn 사용
IO.warn("경고: 제한을 초과하려고 합니다!")
```

`IO.puts/2`에 대한 터미널에서의 샘플 출력:
```
Error: 무언가 잘못되었습니다!
```

`IO.warn/2`의 경우, 출력은 비슷하지만 `IO.warn/2`는 특히 경고용으로 설계되어 있어 향후 Elixir 버전에서 추가적인 형식이나 동작을 포함할 수 있습니다.

**타사 라이브러리 사용하기**

Elixir의 표준 라이브러리만으로 표준 오류 출력을 처리하기에 충분한 경우가 많지만, 더 복잡한 애플리케이션을 위해 또는 다른 로그 수준과 출력을 구성하기 위해 `Logger`와 같은 라이브러리를 유용하게 사용할 수 있습니다.

오류 메시지를 출력하기 위해 `Logger` 사용 예:

```elixir
require Logger

# Logger를 stderr로 출력하도록 구성
Logger.configure_backend(:console, device: :stderr)

# 오류 메시지 작성
Logger.error("Error: 데이터베이스에 연결하지 못했습니다.")
```

이 설정은 `Logger`의 출력을 특정적으로 stderr로 직접하여, 오류 로깅을 표준 로그 메시지와 분리하는 데 유용합니다.
