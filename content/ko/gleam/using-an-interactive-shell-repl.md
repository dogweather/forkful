---
title:                "인터랙티브 셸 (REPL) 사용하기"
date:                  2024-01-26T04:14:30.430137-07:00
model:                 gpt-4-0125-preview
simple_title:         "인터랙티브 셸 (REPL) 사용하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## 무엇인가 & 왜 사용하는가?

REPL은 Read-Eval-Print Loop의 약자로, 코드를 상호작용적으로 실행하고 즉시 결과를 볼 수 있는 프로그래밍 도구입니다. 프로그래머들은 Gleam과 같은 새로운 언어를 현장에서 실험, 디버그 또는 학습하기 위해 이를 사용합니다.

## 사용 방법:

Gleam은 현재 표준 배포판에 REPL을 포함하고 있지 않습니다. 하지만 Gleam이 Erlang 바이트코드로 컴파일되기 때문에, 기존의 Erlang 셸을 사용해 Gleam 코드를 실험할 수 있습니다. 방법은 다음과 같습니다:

1. Gleam 코드를 Erlang으로 컴파일합니다.
```plaintext
gleam build
```

2. Erlang 셸을 시작합니다.
```plaintext
erl -pa ebin
```

3. Gleam 함수를 호출합니다 (모듈 이름이 `my_mod`이고 함수 이름이 `my_fun`이라고 가정).
```erlang
my_mod:my_fun().
```

셸에서 함수의 출력을 볼 수 있어야 합니다.

## 심층 다이브

REPL은 LISP의 1960년대 REPL에서 시작되어 많은 함수형 프로그래밍 언어의 동적이고 탐색적인 정신을 구현합니다. 비교적으로, 파이썬의 `ipython`이나 루비의 `irb`와 같은 다른 시스템들도 자신들의 커뮤니티를 위해 비슷한 경험을 제공합니다.

Gleam에는 아직 네이티브 REPL이 없지만, Erlang 셸을 활용하는 것은 깔끔한 우회로입니다. Erlang 셸의 기능은 BEAM VM, 즉 Erlang 생태계, Elixir, LFE, 그리고 Gleam을 지원하는 가상 머신에서 비롯됩니다.

Gleam 생태계에서 REPL의 대안은 전체 프로젝트 설정 외부에서 코드 조각을 테스트하기 위해 테스트 케이스를 작성하거나 Gleam을 지원하는 온라인 컴파일러와 코드 플레이그라운드를 사용하는 것을 포함할 수 있습니다.

전용 Gleam REPL의 구현은 주로 Gleam과 Erlang의 런타임이 컴파일된 성격에서 도전을 마주칩니다. 여기서 핫 코드 스와핑은 일반적입니다. 어떤 미래의 Gleam REPL은 언어의 정적 타이핑을 REPL이 기대하는 동적 실행 환경과 조화를 이루어야 할 필요가 있습니다.

## 참고

- Gleam 공식 문서: https://gleam.run/book/
- Erlang 셸 문서: http://erlang.org/doc/man/erl.html
- 온라인 Gleam 컴파일러 플레이그라운드: https://gleam.run/compiler/