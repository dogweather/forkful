---
date: 2024-01-26 04:13:17.238943-07:00
description: "\uB300\uD654\uD615 \uC258 \uB610\uB294 REPL(Read-Eval-Print Loop)\uC740\
  \ \uC2E4\uC2DC\uAC04\uC73C\uB85C \uCF54\uB4DC \uC2A4\uB2C8\uD3AB\uC744 \uC2DC\uB3C4\
  \uD574 \uBCFC \uC218 \uC788\uAC8C \uD574 \uC90D\uB2C8\uB2E4. \uC5D8\uB9AD\uC11C\
  \ \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 IEx(Interactive Elixir)\uB77C\uACE0\
  \ \uBD88\uB9AC\uB294 REPL\uC744 \uC2E4\uD5D8, \uB514\uBC84\uAE45 \uBC0F \uC5B8\uC5B4\
  \ \uD559\uC2B5\uC744 \uC704\uD574 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.723973-06:00'
model: gpt-4-0125-preview
summary: "\uB300\uD654\uD615 \uC258 \uB610\uB294 REPL(Read-Eval-Print Loop)\uC740\
  \ \uC2E4\uC2DC\uAC04\uC73C\uB85C \uCF54\uB4DC \uC2A4\uB2C8\uD3AB\uC744 \uC2DC\uB3C4\
  \uD574 \uBCFC \uC218 \uC788\uAC8C \uD574 \uC90D\uB2C8\uB2E4. \uC5D8\uB9AD\uC11C\
  \ \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 IEx(Interactive Elixir)\uB77C\uACE0\
  \ \uBD88\uB9AC\uB294 REPL\uC744 \uC2E4\uD5D8, \uB514\uBC84\uAE45 \uBC0F \uC5B8\uC5B4\
  \ \uD559\uC2B5\uC744 \uC704\uD574 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uC778\uD130\uB799\uD2F0\uBE0C \uC178 (REPL) \uC0AC\uC6A9\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇인가 & 왜인가?
대화형 쉘 또는 REPL(Read-Eval-Print Loop)은 실시간으로 코드 스니펫을 시도해 볼 수 있게 해 줍니다. 엘릭서 프로그래머들은 IEx(Interactive Elixir)라고 불리는 REPL을 실험, 디버깅 및 언어 학습을 위해 사용합니다.

## 어떻게 사용하는가:
IEx를 실행하려면 터미널을 열고 `iex`를 입력하십시오. 맛보기입니다:

```Elixir
iex> name = "Elixir Programmer"
"Elixir Programmer"
iex> String.length(name)
17
iex> Enum.map([1, 2, 3], fn num -> num * 3 end)
[3, 6, 9]
```

출력에는 변수 할당, 함수 결과, 그리고 익명 함수 작동이 표시됩니다.

## 깊이 있게 탐구하기
IEx 쉘은 엘릭서의 초기부터 일부였습니다. 엘릭서의 창시자인 José Valim은 파이썬의 `python`과 루비의 `irb` 같은 다른 언어들의 대화형 쉘에서 영감을 받았습니다. IEx는 이러한 언어들과 많은 기능을 공유하면서도, 엘릭서의 동시성적 특성을 처리하도록 설계되었으며, Erlang VM 기능과 완전히 통합되어 있습니다.

Erlang 생태계에서 IEx의 대안으로는 Erlang 쉘이 있는 `erl`이 있습니다. 그러나 IEx는 포괄적인 탭 완성, 히스토리, 도우미 같은 기능과 함께, 더 엘릭서 친화적인 환경을 제공합니다.

IEx REPL은 단지 놀이터가 아니라, 실행 중인 시스템에 원활하게 연결할 수 있습니다. 이는 실시간 애플리케이션의 디버깅에 있어 중요합니다. 기본 구현은 BEAM(에를랑 VM)에 의존하며, 쉘 내에서 핫 코드 스와핑 같은 기능을 지원합니다.

## 또한 보십시오
추가적인 독서 및 자료를 위해 다음을 확인하십시오:

- [엘릭서의 IEx 문서](https://hexdocs.pm/iex/IEx.html)
- [대화형 엘릭서 (IEx) - 엘릭서 쉘](https://elixir-lang.org/getting-started/introduction.html#interactive-elixir)
- [Erlang의 `erl` 문서](http://erlang.org/doc/man/erl.html)
- [엘릭서의 대화형 쉘 배우기](https://elixirschool.com/en/lessons/basics/iex_helpers/)
