---
date: 2024-01-26 01:18:48.718079-07:00
description: "\uB9AC\uD329\uD1A0\uB9C1\uC740 \uCF54\uB4DC\uC758 \uC678\uBD80 \uB3D9\
  \uC791\uC744 \uBCC0\uACBD\uD558\uC9C0 \uC54A\uACE0 \uAE30\uC874 \uCF54\uB4DC\uB97C\
  \ \uC7AC\uAD6C\uC870\uD654\uD558\uB294 \uACFC\uC815\uC73C\uB85C, \uAC00\uB3C5\uC131\
  \uACFC \uC720\uC9C0\uBCF4\uC218\uC131 \uAC19\uC740 \uBE44\uAE30\uB2A5\uC801 \uC18D\
  \uC131\uC744 \uAC1C\uC120\uD558\uB294 \uAC83\uC744 \uBAA9\uD45C\uB85C \uD569\uB2C8\
  \uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB294 \uCF54\uB4DC\uB97C \uB354 \uAE68\uB057\
  \uD558\uACE0 \uC774\uD574\uD558\uAE30 \uC27D\uAC8C, \uADF8\uB9AC\uACE0 \uD6A8\uC728\
  \uC801\uC73C\uB85C \uB9CC\uB4E4\uC5B4 \uBBF8\uB798\uC758 \uC5C5\uB370\uC774\uD2B8\
  \uB97C \uC6A9\uC774\uD558\uAC8C \uD558\uACE0 \uBC84\uADF8\uC758 \uC704\uD5D8\uC744\
  \u2026"
lastmod: '2024-03-13T22:44:54.734064-06:00'
model: gpt-4-0125-preview
summary: "\uB9AC\uD329\uD1A0\uB9C1\uC740 \uCF54\uB4DC\uC758 \uC678\uBD80 \uB3D9\uC791\
  \uC744 \uBCC0\uACBD\uD558\uC9C0 \uC54A\uACE0 \uAE30\uC874 \uCF54\uB4DC\uB97C \uC7AC\
  \uAD6C\uC870\uD654\uD558\uB294 \uACFC\uC815\uC73C\uB85C, \uAC00\uB3C5\uC131\uACFC\
  \ \uC720\uC9C0\uBCF4\uC218\uC131 \uAC19\uC740 \uBE44\uAE30\uB2A5\uC801 \uC18D\uC131\
  \uC744 \uAC1C\uC120\uD558\uB294 \uAC83\uC744 \uBAA9\uD45C\uB85C \uD569\uB2C8\uB2E4\
  ."
title: "\uB9AC\uD329\uD1A0\uB9C1"
weight: 19
---

## 무엇 & 왜?
리팩토링은 코드의 외부 동작을 변경하지 않고 기존 코드를 재구조화하는 과정으로, 가독성과 유지보수성 같은 비기능적 속성을 개선하는 것을 목표로 합니다. 프로그래머는 코드를 더 깨끗하고 이해하기 쉽게, 그리고 효율적으로 만들어 미래의 업데이트를 용이하게 하고 버그의 위험을 줄이기 위해 이 작업을 수행합니다.

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
