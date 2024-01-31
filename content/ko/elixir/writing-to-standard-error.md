---
title:                "표준 오류로 쓰기"
date:                  2024-01-19
html_title:           "Bash: 표준 오류로 쓰기"
simple_title:         "표준 오류로 쓰기"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가 & 왜 사용하는가?)
표준 에러 출력(standard error)은 프로그램 실행 중 발생하는 에러나 경고 메시지를 사용자나 다른 시스템에게 보여주기 위한 수단이다. 프로그래머들은 문제 진단 및 디버깅을 용이하게 하고, 로그 파일에 기록하기 위해 이를 사용한다.

## How to: (방법)
Elixir에서 표준 에러에 메시지를 쓰는 예제입니다.

```elixir
# 예제 1: 표준 에러로 메시지 보내기
IO.puts(:stderr, "에러 발생!")

# 예제 2: 표준 에러로 포맷된 메시지 보내기
IO.write(:stderr, "경고: 잘못된 입력")
```

예제 실행 시에 당신의 셸에 다음과 같이 출력될 겁니다.

```
에러 발생!
경고: 잘못된 입력
```

## Deep Dive (심화 학습)
표준 에러는 UNIX 시스템에서 오래전부터 사용되어 왔다. 세 가지 주요 스트림 중 하나이며, 표준 입력(stdin), 표준 출력(stdout)과 함께 시스템과의 통신을 관리한다. Elixir에서는 `IO` 모듈을 이용해 쉽게 접근 가능하다.

표준 에러를 파일로 리다이렉트하거나 다른 프로세스에 파이프하는 것도 가능하다. 또한, 로깅 라이브러리를 사용해 로그에 메시지를 기록하는 다른 방법들도 있다.

구현 세부사항으로, Elixir는 Erlang VM 위에서 동작하며, 이는 더 낮은 수준의 OS 기능과 상호작용을 Erlang의 기능을 통해 처리한다.

## See Also (참고 자료)
- Elixir 공식 문서의 `IO` 모듈: [https://hexdocs.pm/elixir/IO.html](https://hexdocs.pm/elixir/IO.html)
- Elixir 소개: [https://elixir-lang.org/getting-started/introduction.html](https://elixir-lang.org/getting-started/introduction.html)
- UNIX 표준 스트림과의 작업 방법에 대한 더 깊은 이해를 위해: [https://en.wikipedia.org/wiki/Standard_streams](https://en.wikipedia.org/wiki/Standard_streams)
