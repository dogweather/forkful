---
title:                "숫자 반올림"
date:                  2024-02-01T22:01:14.332290-07:00
model:                 gpt-4-0125-preview
simple_title:         "숫자 반올림"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/vba/rounding-numbers.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇이며 왜인가?

프로그래밍에서 숫자를 반올림하는 것은 숫자를 가장 가까운 정수 또는 특정 소수점 자리수로 근사하는 것에 관한 것입니다. 프로그래머들은 숫자를 반올림하여 숫자를 단순화시키고, 가독성을 향상시키거나, 특히 정밀도가 중요한 금융 계산에서 특정 숫자 기준을 충족시키기 위해 사용합니다.

## 방법:

Visual Basic for Applications(VBA)에서, 몇 가지 함수를 사용하여 반올림을 수행할 수 있으며, 각각은 특정 시나리오에 적합합니다. 다음은 예제와 함께 가장 일반적으로 사용되는 함수들입니다:

1. **Round 함수**:
   `Round` 함수는 지정된 자릿수만큼 숫자를 반올림합니다.
   ```basic
   Dim roundedNumber As Double
   roundedNumber = Round(3.14159, 2)  ' 출력: 3.14
   MsgBox roundedNumber
   ```
   
2. **Int 및 Fix 함수**:
   `Int`와 `Fix` 함수는 모두 숫자를 가장 가까운 정수로 내림하지만, 음수에 대해 다르게 동작합니다.
   ```basic
   Dim intRounded As Integer
   Dim fixRounded As Integer
   
   intRounded = Int(-3.14159)  ' 출력: -4
   fixRounded = Fix(-3.14159)  ' 출력: -3
   
   MsgBox "Int: " & intRounded & ", Fix: " & fixRounded
   ```

3. **Ceiling 및 Floor 함수**:
   VBA는 다른 언어에서 찾을 수 있는 내장 `Ceiling`과 `Floor` 함수를 가지고 있지 않습니다. 이를 시뮬레이션하려면, Excel VBA에서 `Application.WorksheetFunction.Ceiling_Math`와 `Application.WorksheetFunction.Floor_Math`를 사용합니다.
   ```basic
   Dim ceilingNumber As Double
   Dim floorNumber As Double
   
   ceilingNumber = Application.WorksheetFunction.Ceiling_Math(3.14159)  ' 출력: 4
   floorNumber = Application.WorksheetFunction.Floor_Math(3.14159)  ' 출력: 3
   
   MsgBox "Ceiling: " & ceilingNumber & ", Floor: " & floorNumber
   ```

## 심층 분석

VBA의 `Round` 함수는 **은행가의 반올림**을 사용하기 때문에 다른 언어의 반올림 방법과 본질적으로 다릅니다. 은행가의 반올림은 두 숫자 사이의 정확히 중간일 때 가장 가까운 짝수로 반올림하여, 큰 데이터 셋에서의 계산 결과에 대한 편향을 줄이고 더 통계적으로 유의미한 결과를 제공합니다. 그러나 이것은 그것에 익숙하지 않은 사람들, 특히 모든 경우에 정수 정밀도가 기대될 때 예상치 못한 동작을 야기할 수 있습니다.

반면에, 많은 프로그래밍 언어와 시스템은 "산술 반올림" 또는 "반올림"을 사용하는데, 이는 두 개의 가능한 반올림된 값 사이의 정확히 중간에 있는 숫자는 항상 올림됩니다. 다른 언어에서 VBA로 코드를 번역하거나 이식할 때, 프로그래머는 금융 및 통계 애플리케이션에서 미묘한 버그나 부정확성을 피하기 위해 이러한 차이를 염두에 두어야 합니다.

VBA는 반올림을 위한 다양한 함수를 제공하지만, `Ceiling`과 `Floor` 함수의 부재(Excel의 WorksheetFunction을 이용하지 않고서는)는 그것의 기본 기능의 한계를 드러냅니다. 기능이 풍부한 언어에서 온 프로그래머들은 이러한 생략을 불편하게 여길 수 있으며, 사용 가능한 함수를 이용해 계산을 적용하거나 맞춤 솔루션을 구현해야 할 필요가 있을 수 있습니다. 이러한 한계에도 불구하고, VBA의 반올림 기능을 올바르게 이해하고 사용하는 것은 수치 계산이 정확하고 대부분의 애플리케이션 요구 사항을 충족하도록 도와줄 수 있습니다.
