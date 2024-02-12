---
title:                "오류 처리하기"
aliases:
- ko/vba/handling-errors.md
date:                  2024-02-01T21:55:01.186665-07:00
model:                 gpt-4-0125-preview
simple_title:         "오류 처리하기"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/vba/handling-errors.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇이며 왜?

Visual Basic for Applications(VBA)의 오류 처리는 프로그래밍, 애플리케이션 또는 통신 오류를 예측, 감지 및 해결하는 과정을 말합니다. 강력한 오류 처리를 구현하는 것은 애플리케이션의 무결성을 유지하고 예기치 않은 문제를 우아하게 관리하여 갑작스러운 충돌이나 데이터 손실 없이 사용자 경험을 향상시키는 데 중요합니다.

## 방법:

VBA에서는 `On Error` 문을 사용하여 오류가 발생했을 때 VBA가 어떻게 진행될지 지시하여 일반적으로 오류 처리를 구현합니다. 가장 일반적인 오류 처리 전략에는 `On Error GoTo` 레이블, `On Error Resume Next`, 그리고 `On Error GoTo 0`이 포함됩니다.

**예제 1: `On Error GoTo` 사용하기**

이 접근 방식을 통해 오류가 발생한 직후 특정 코드 섹션으로 프로그램을 직접 지시할 수 있습니다.

```vb
Sub ErrorHandlerExample()
    On Error GoTo ErrHandler
    Dim intDivision As Integer

    intDivision = 5 / 0 ' 이것은 0으로 나누기 오류를 일으킬 것입니다.

    Exit Sub
ErrHandler:
    MsgBox "오류가 발생했습니다: " & Err.Description, vbCritical, "오류!"
    Resume Next
End Sub
```

이 예제에서는 런타임 오류가 발생하면 `ErrHandler`로 점프하여 오류 메시지를 표시한 다음 오류 후 다음 줄로 진행합니다.

**예제 2: `On Error Resume Next` 사용하기**

이 전략은 오류가 발생하더라도 VBA가 다음 코드 줄을 계속 실행하도록 지시합니다. 이는 예상되는 오류가 해로울 것으로 예상되지 않거나 나중에 실행 중 오류를 처리할 계획일 때 유용할 수 있습니다.

```vb
Sub ResumeNextExample()
    On Error Resume Next
    Dim intDivision As Integer
    intDivision = 5 / 0 ' 이것은 프로그램을 멈추지 않게 하며; 오류는 무시됩니다.
    
    ' 오류가 발생했는지 확인
    If Err.Number <> 0 Then
        MsgBox "오류가 발생했습니다: " & Err.Description, vbExclamation, "처리된 오류"
        ' 오류 재설정
        Err.Clear
    End If
End Sub
```

이 경우, 프로그램은 오류에 대해 중단되지 않지만 오류가 발생했는지 확인하고 발생한 경우 처리한 다음 오류를 지웁니다.

## 심층 탐구

역사적으로, 프로그래밍 언어에서 오류 처리는 간단한 goto 문에서 시작하여 Java 및 C#과 같은 언어에서의 예외와 같은 더 정교한 메커니즘으로 발전했습니다. VBA의 오류 처리는 현대의 예외 처리만큼 강력하거나 유연하지는 않지만, Microsoft Office 환경에서 작업을 자동화하는 언어의 적용 맥락에서 그 목적에 부합합니다.

VBA의 오류 처리의 주요 한계는 다소 번거롭고 수동적인 접근 방식에 있으며, 오류 처리 코드의 신중한 배치와 실행의 흐름에 대한 명확한 이해가 필요합니다. 현대 프로그래밍 언어는 일반적으로 수동적인 검사나 코드 실행 중의 점프 없이 오류 처리 코드로의 흐름을 자동으로 처리하는 try-catch 블록과 같은 더 우아한 해결책을 제공합니다.

이러한 한계에도 불구하고, VBA의 오류 처리 메커니즘은 대부분의 자동화 작업에 적합하며, 제대로 사용되면 사용자에게 문제를 일으키는 처리되지 않은 오류의 가능성을 상당히 줄일 수 있습니다. 또한, VBA의 오류 처리를 이해하는 것은 소프트웨어 개발에서 오류 처리 전략의 진화와 과거 프로그래밍 패러다임에 대한 통찰력을 제공할 수 있습니다.
