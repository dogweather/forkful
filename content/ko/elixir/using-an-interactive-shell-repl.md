---
title:                "인터랙티브 셸 (REPL) 사용하기"
aliases:
- ko/elixir/using-an-interactive-shell-repl.md
date:                  2024-01-26T04:13:17.238943-07:00
model:                 gpt-4-0125-preview
simple_title:         "인터랙티브 셸 (REPL) 사용하기"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/using-an-interactive-shell-repl.md"
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
