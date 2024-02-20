---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:06.372138-07:00
description: "Elixir\uC5D0\uC11C \uD45C\uC900 \uC624\uB958(stderr)\uB85C \uC4F0\uAE30\
  \uB294 \uC624\uB958 \uBA54\uC2DC\uC9C0\uC640 \uC9C4\uB2E8\uC744 \uC8FC \uCD9C\uB825\
  (stdout)\uACFC \uBD84\uB9AC\uD558\uC5EC \uC9C1\uC811\uD558\uB294 \uBC29\uBC95\uC785\
  \uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB294 stderr\uB97C \uC0AC\uC6A9\uD558\
  \uC5EC \uD504\uB85C\uADF8\uB7A8\uC758 \uC8FC \uCD9C\uB825\uC744 \uD63C\uB780\uC2A4\
  \uB7FD\uAC8C \uD558\uC9C0 \uC54A\uACE0 \uC624\uB958\uB97C \uB514\uBC84\uADF8\uD558\
  \uACE0 \uCC98\uB9AC\uD568\uC73C\uB85C\uC368 \uBB38\uC81C\uB97C \uC27D\uAC8C \uC2DD\
  \uBCC4\uD558\uACE0 \uD574\uACB0\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
lastmod: 2024-02-19 22:05:13.693687
model: gpt-4-0125-preview
summary: "Elixir\uC5D0\uC11C \uD45C\uC900 \uC624\uB958(stderr)\uB85C \uC4F0\uAE30\uB294\
  \ \uC624\uB958 \uBA54\uC2DC\uC9C0\uC640 \uC9C4\uB2E8\uC744 \uC8FC \uCD9C\uB825(stdout)\uACFC\
  \ \uBD84\uB9AC\uD558\uC5EC \uC9C1\uC811\uD558\uB294 \uBC29\uBC95\uC785\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB294 stderr\uB97C \uC0AC\uC6A9\uD558\uC5EC \uD504\
  \uB85C\uADF8\uB7A8\uC758 \uC8FC \uCD9C\uB825\uC744 \uD63C\uB780\uC2A4\uB7FD\uAC8C\
  \ \uD558\uC9C0 \uC54A\uACE0 \uC624\uB958\uB97C \uB514\uBC84\uADF8\uD558\uACE0 \uCC98\
  \uB9AC\uD568\uC73C\uB85C\uC368 \uBB38\uC81C\uB97C \uC27D\uAC8C \uC2DD\uBCC4\uD558\
  \uACE0 \uD574\uACB0\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uD45C\uC900 \uC5D0\uB7EC\uC5D0 \uC4F0\uAE30"
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
