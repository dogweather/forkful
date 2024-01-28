---
title:                "숫자 반올림하기"
date:                  2024-01-26T03:44:02.376206-07:00
model:                 gpt-4-0125-preview
simple_title:         "숫자 반올림하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/rounding-numbers.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
숫자를 반올림하는 것은 단순화하거나 특정한 정밀도에 맞추기 위해 그 값들을 가까운 값으로 조정하는 것을 말합니다. 이는 가독성을 향상시키거나 저장 공간을 줄이거나, 가장 가까운 센트로 반올림하고 싶은 돈 계산과 같은 도메인 특정 요구 사항을 충족시키는 데 유용합니다.

## 방법:
Elixir에서는 `Float.round/2`를 사용하여 부동 소수점 숫자를 반올림할 수 있습니다. 유지하고 싶은 소수점 자릿수를 지정할 수 있습니다. 작동 방식은 다음과 같습니다:

```elixir
# 소수점 없이 숫자 반올림
Float.round(3.14159) # => 3.0

# 소수점 2자리까지 숫자 반올림
Float.round(3.14159, 2) # => 3.14

# 소수점을 음수 정밀도로 반올림하여 가장 가까운 10으로 반올림
Float.round(123.456, -1) # => 120.0
```

## 깊이 알아보기
숫자를 반올림하는 것은 컴퓨터 과학에서 고전적인 문제입니다. 그만큼 반올림 전략의 선택은 금융 시스템, 과학 계산 등에 영향을 미칠 수 있습니다. Elixir의 `Float.round/2`는 수학 수업에서 배운 전통적인 반올림, 즉 "반올림"을 기본으로 합니다.

다른 유형의 반올림이 필요한 경우, Elixir는 직접 만들 수 있습니다. 예를 들어, "floor" 반올림(항상 내림) 또는 "ceiling" 반올림(항상 올림)을 고려해 보세요. 각각 `Float.floor/1` 또는 `Float.ceil/1`을 사용하면 됩니다.

```elixir
# Floor 반올림
Float.floor(3.999) # => 3.0

# Ceiling 반올림
Float.ceil(3.001) # => 4.0
```

이러한 대안들은 금융 계산, 그래픽 렌더링 또는 데이터 근사와 같은 애플리케이션의 정확한 요구 사항에 맞게 반올림을 맞춤화하는 데 도움이 됩니다.

## 또한 보기
Elixir의 반올림 함수와 부동 소수점 숫자에 대한 자세한 내용은:

- Elixir 공식 문서 중 `Float`: https://hexdocs.pm/elixir/Float.html
- 부동 소수점 산술에 대한 IEEE 표준 (IEEE 754): https://ieeexplore.ieee.org/document/4610935
