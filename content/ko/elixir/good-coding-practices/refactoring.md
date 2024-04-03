---
date: 2024-01-26 01:18:48.718079-07:00
description: "\uBC29\uBC95: \uC77C\uBC18\uC801\uC778 Elixir \uD328\uD134\uC744 \uC815\
  \uB9AC\uD574\uBD05\uC2DC\uB2E4. `calculate_stats` \uD568\uC218\uB97C \uC791\uACE0\
  \ \uC7AC\uC0AC\uC6A9 \uAC00\uB2A5\uD55C \uC5EC\uB7EC \uC870\uAC01\uC73C\uB85C \uB098\
  \uB204\uC5B4 \uB9AC\uD329\uD1A0\uB9C1\uD560 \uAC83\uC785\uB2C8\uB2E4. \uC774 \uD568\
  \uC218\uB294 \uD604\uC7AC \uD558\uB098\uC758 \uD568\uC218\uAC00 \uB9E1\uC544\uC11C\
  \uB294 \uC548 \uB420 \uB108\uBB34 \uB9CE\uC740 \uC77C\uC744 \uD558\uACE0 \uC788\uC2B5\
  \uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.734064-06:00'
model: gpt-4-0125-preview
summary: "\uC77C\uBC18\uC801\uC778 Elixir \uD328\uD134\uC744 \uC815\uB9AC\uD574\uBD05\
  \uC2DC\uB2E4."
title: "\uB9AC\uD329\uD1A0\uB9C1"
weight: 19
---

## 방법:
일반적인 Elixir 패턴을 정리해봅시다. `calculate_stats` 함수를 작고 재사용 가능한 여러 조각으로 나누어 리팩토링할 것입니다. 이 함수는 현재 하나의 함수가 맡아서는 안 될 너무 많은 일을 하고 있습니다.

```elixir
defmodule Stats do
  # 원본, 리팩토링되지 않은 코드
  def calculate_stats(data) do
    총합 = Enum.sum(data)
    개수 = Enum.count(data)
    평균 = 총합 / 개수
    {평균, 총합}
  end
  
  # 리팩토링된 코드
  def calculate_mean(data), do: Enum.sum(data) / Enum.count(data)
  
  def calculate_total(data), do: Enum.sum(data)
  
  def calculate_stats_refactored(data) do
    평균 = calculate_mean(data)
    총합 = calculate_total(data)
    {평균, 총합}
  end
end

# 예시 출력
# 리팩토링 전
Stats.calculate_stats([1, 2, 3])
# => {2.0, 6}

# 리팩토링 후
Stats.calculate_stats_refactored([1, 2, 3])
# => {2.0, 6}
```
보다시피, 출력 결과는 동일하나 이제 독립적으로 재사용하고 테스트할 수 있는 모듈식 함수를 갖게 되었습니다.

## 심층 분석
리팩토링은 새로운 개념이 아니며, 소프트웨어 개발 초기부터 프로그래밍의 중요한 부분이 되었습니다. Martin Fowler의 "Refactoring: Improving the Design of Existing Code" 같은 주목할 만한 작업들은 언제, 어떻게 리팩토링을 적용해야 하는지에 대한 통찰력을 제공하는 리팩토링의 기초적인 실습을 제공합니다.

수동 리팩토링 대안으로는 리팩토링을 제안하거나 실제로 수행할 수 있는 자동 코드 분석 도구가 있습니다. 그러나 자동화된 도구는 코드의 전체 맥락을 항상 파악하지 못하고 인간 검토자가 포착할 수 있는 미묘한 차이를 놓칠 수 있습니다.

Elixir에서의 구현 세부 사항은 함수형 패러다임을 이해하고 패턴 매칭, 가드 절, 파이프 연산자를 활용하여 명확하고 간결한 코드를 작성하는 것을 포함합니다. 예를 들어, 리팩토링은 종종 복잡한 명령형 스타일의 함수를 Elixir의 불변성과 부작용이 없는 작업을 선호하는 더 작고 조합 가능한 함수로 변환하는 것을 포함합니다.

## 참고자료
Elixir 특정 리팩토링 기술에 대한 자세한 내용은:

- [Elixir의 공식 가이드](https://elixir-lang.org/getting-started/)
- ["Refactoring: Improving the Design of Existing Code" by Martin Fowler](https://martinfowler.com/books/refactoring.html), Elixir에 적용할 수 있는 일반 원칙을 위해
- [Credo, Elixir를 위한 정적 코드 분석 도구](https://github.com/rrrene/credo)는 모범 사례를 장려합니다.
- [Exercism Elixir 트랙](https://exercism.org/tracks/elixir), 리팩토링을 포함한 실제 연습에 대해
