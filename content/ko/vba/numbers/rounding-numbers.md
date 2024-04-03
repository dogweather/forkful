---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:01:14.332290-07:00
description: "\uD504\uB85C\uADF8\uB798\uBC0D\uC5D0\uC11C \uC22B\uC790\uB97C \uBC18\
  \uC62C\uB9BC\uD558\uB294 \uAC83\uC740 \uC22B\uC790\uB97C \uAC00\uC7A5 \uAC00\uAE4C\
  \uC6B4 \uC815\uC218 \uB610\uB294 \uD2B9\uC815 \uC18C\uC218\uC810 \uC790\uB9AC\uC218\
  \uB85C \uADFC\uC0AC\uD558\uB294 \uAC83\uC5D0 \uAD00\uD55C \uAC83\uC785\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC22B\uC790\uB97C \uBC18\uC62C\uB9BC\
  \uD558\uC5EC \uC22B\uC790\uB97C \uB2E8\uC21C\uD654\uC2DC\uD0A4\uACE0, \uAC00\uB3C5\
  \uC131\uC744 \uD5A5\uC0C1\uC2DC\uD0A4\uAC70\uB098, \uD2B9\uD788 \uC815\uBC00\uB3C4\
  \uAC00 \uC911\uC694\uD55C \uAE08\uC735 \uACC4\uC0B0\uC5D0\uC11C \uD2B9\uC815 \uC22B\
  \uC790 \uAE30\uC900\uC744 \uCDA9\uC871\uC2DC\uD0A4\uAE30 \uC704\uD574 \uC0AC\uC6A9\
  \uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.972842-06:00'
model: gpt-4-0125-preview
summary: "\uD504\uB85C\uADF8\uB798\uBC0D\uC5D0\uC11C \uC22B\uC790\uB97C \uBC18\uC62C\
  \uB9BC\uD558\uB294 \uAC83\uC740 \uC22B\uC790\uB97C \uAC00\uC7A5 \uAC00\uAE4C\uC6B4\
  \ \uC815\uC218 \uB610\uB294 \uD2B9\uC815 \uC18C\uC218\uC810 \uC790\uB9AC\uC218\uB85C\
  \ \uADFC\uC0AC\uD558\uB294 \uAC83\uC5D0 \uAD00\uD55C \uAC83\uC785\uB2C8\uB2E4."
title: "\uC22B\uC790 \uBC18\uC62C\uB9BC"
weight: 13
---

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
