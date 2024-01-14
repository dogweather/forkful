---
title:                "Elixir: 표준 에러로 쓰기"
simple_title:         "표준 에러로 쓰기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

### 왜?

에릭서 프로그래밍은 익숙해지기까지는 조금 어렵습니다. 하지만 한번 익숙해지면, Elixir는 매우 효율적이고 생산적인 언어로 자리잡았습니다. 개발자들은 현재도 이 언어를 배우고 사용하는 것을 두려워하지 않아야합니다. 에릭서 표준 오류에 대해 쓰는 것은 에릭서 코드를 더욱 에러에 대해 안전하고 확장 가능하게 만들어줍니다. 

### 방법

이 글에서는 Elixir에서 표준 오류에 쓰는 방법을 알려드리겠습니다. 먼저 `IO` 모듈의 하나인 `stderr` 함수를 사용해 에러를 표준 오류에 쓰는 방법을 배워보겠습니다. 

```Elixir
IO.stderr("Something went wrong!")
```

위 코드를 실행하면 어떤 일이 일어나는지 알아봅시다.

### 심화 분석

에릭서에서 `IO.stderr` 함수는 문자열을 매개변수로 받고, 이를 표준 오류에 써주는 역할을 합니다. 이는 `IO.puts` 함수와 같은 원리로 작동합니다. 하지만 `IO.stderr` 함수는 오류를 표시하는데 사용되는 채널처럼 표준 출력과는 별도로 처리됩니다. 따라서 에러를 잡는데 유용하며, 에러 메시지를 표준 출력과 구분할 수 있게 합니다.

### 관련 자료

- [Elixir 공식 문서](https://hexdocs.pm/elixir/IO.html#stderr/2)
- [파이썬에서 표준 오류 쓰기](https://www.programcreek.com/python/index/9407/logging.Logger.error)
- [Bash에서 표준 에러 출력하기](https://unix.stackexchange.com/questions/8656/how-to-forward-errors-to-log-file)