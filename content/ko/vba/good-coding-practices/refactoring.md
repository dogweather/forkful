---
aliases:
- /ko/vba/refactoring/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:00:36.442687-07:00
description: "\uD504\uB85C\uADF8\uB798\uBC0D\uC5D0\uC11C \uB9AC\uD329\uD130\uB9C1\uC740\
  \ \uCF54\uB4DC\uC758 \uAD6C\uC870\uB97C \uBCC0\uACBD\uD558\uC9C0 \uC54A\uACE0 \uADF8\
  \uAC83\uC758 \uB3D9\uC791\uC744 \uBCC0\uACBD\uD558\uC9C0 \uC54A\uACE0 \uCF54\uB4DC\
  \uC758 \uAC00\uB3C5\uC131, \uC720\uC9C0 \uAD00\uB9AC\uC131 \uB610\uB294 \uC131\uB2A5\
  \uACFC \uAC19\uC740 \uCE21\uBA74\uC744 \uAC1C\uC120\uD558\uAE30 \uC704\uD574 \uCF54\
  \uB4DC \uAD6C\uC870\uB97C \uC218\uC815\uD558\uB294 \uAC83\uC744 \uD3EC\uD568\uD569\
  \uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uCF54\uB4DC\uB97C \uB354\
  \ \uD6A8\uC728\uC801\uC73C\uB85C \uB9CC\uB4E4\uACE0, \uC774\uD574\uD558\uAE30 \uC27D\
  \uAC8C \uD558\uBA70, \uBBF8\uB798\uC5D0 \uC218\uC815\uD558\uAE30\u2026"
lastmod: 2024-02-18 23:09:05.966780
model: gpt-4-0125-preview
summary: "\uD504\uB85C\uADF8\uB798\uBC0D\uC5D0\uC11C \uB9AC\uD329\uD130\uB9C1\uC740\
  \ \uCF54\uB4DC\uC758 \uAD6C\uC870\uB97C \uBCC0\uACBD\uD558\uC9C0 \uC54A\uACE0 \uADF8\
  \uAC83\uC758 \uB3D9\uC791\uC744 \uBCC0\uACBD\uD558\uC9C0 \uC54A\uACE0 \uCF54\uB4DC\
  \uC758 \uAC00\uB3C5\uC131, \uC720\uC9C0 \uAD00\uB9AC\uC131 \uB610\uB294 \uC131\uB2A5\
  \uACFC \uAC19\uC740 \uCE21\uBA74\uC744 \uAC1C\uC120\uD558\uAE30 \uC704\uD574 \uCF54\
  \uB4DC \uAD6C\uC870\uB97C \uC218\uC815\uD558\uB294 \uAC83\uC744 \uD3EC\uD568\uD569\
  \uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uCF54\uB4DC\uB97C \uB354\
  \ \uD6A8\uC728\uC801\uC73C\uB85C \uB9CC\uB4E4\uACE0, \uC774\uD574\uD558\uAE30 \uC27D\
  \uAC8C \uD558\uBA70, \uBBF8\uB798\uC5D0 \uC218\uC815\uD558\uAE30\u2026"
title: "\uB9AC\uD329\uD1A0\uB9C1"
---

{{< edit_this_page >}}

## 무엇 & 왜?

프로그래밍에서 리팩터링은 코드의 구조를 변경하지 않고 그것의 동작을 변경하지 않고 코드의 가독성, 유지 관리성 또는 성능과 같은 측면을 개선하기 위해 코드 구조를 수정하는 것을 포함합니다. 프로그래머들은 코드를 더 효율적으로 만들고, 이해하기 쉽게 하며, 미래에 수정하기 쉽고, 버그 가능성을 줄이기 위해 리팩터링을 합니다.

## 방법:

Visual Basic for Applications(VBA)에서 기본적인 예를 고려해 보겠습니다. 우리는 직원의 세부 정보를 출력하는 서브루틴을 가지고 있습니다. 처음에는 코드가 지저분하고 유지 관리하거나 확장하기 어렵습니다.

```vb
Sub PrintEmployeeDetails()
    Dim name As String
    Dim age As Integer
    Dim department As String
    name = "John Doe"
    age = 30
    department = "IT"
    
    MsgBox "Name: " & name & vbCrLf & "Age: " & age & vbCrLf & "Department: " & department
End Sub
```

리팩터링 단계 1: 메소드 추출. 가장 일반적인 리팩터링 기술 중 하나는 특정 코드 조각을 가져다가 자체 메소드로 이동시키는 것입니다. 이렇게 하면 코드가 더 모듈화되고 이해하기 쉬워집니다.

```vb
Sub PrintEmployeeDetails()
    Dim name As String
    Dim age As Integer
    Dim department As String
    name = "John Doe"
    age = 30
    department = "IT"
    
    DisplayMessage name, age, department
End Sub

Private Sub DisplayMessage(name As String, age As Integer, department As String)
    MsgBox "Name: " & name & vbCrLf & "Age: " & age & vbCrLf & "Department: " & department
End Sub
```

리팩터링 단계 2: 구조체 사용하기. 이 단계는 관련 데이터를 담기 위한 데이터 구조를 사용하는 것을 포함해, 코드의 명확성을 개선하고 그룹화된 데이터를 쉽게 전달할 수 있게 만듭니다.

```vb
Type Employee
    name As String
    age As Integer
    department As String
End Type

Sub PrintEmployeeDetails()
    Dim emp As Employee
    emp.name = "John Doe"
    emp.age = 30
    emp.department = "IT"
    
    DisplayMessage emp
End Sub

Private Sub DisplayMessage(emp As Employee)
    MsgBox "Name: " & emp.name & vbCrLf & "Age: " & emp.age & vbCrLf & "Department: " & emp.department
End Sub
```

이 단계들은 지저분한 코드를 모듈화되고 구조화된 코드로 변환하여 가독성과 유지 보수성을 크게 개선합니다.

## 심층 분석

리팩터링이라는 개념은 프로그래밍이 시작될 때부터 있었지만, 마틴 파울러의 책 "리팩터링: 기존 코드의 설계를 개선하는 방법"이 그것을 대중에게 알리고 소프트웨어 개발 과정에서 그 중요성을 강조했습니다. Visual Basic for Applications에서, 리팩터링은 자동 리팩터링을 지원하는 더 현대적인 통합 개발 환경(IDE)에서 찾을 수 있는 내장된 도구가 부족하기 때문에 다소 도전적일 수 있습니다.

그러나 이는 그 중요성을 감소시키지 않습니다. VBA에서도 기본 리팩터링 기술을 수동으로 적용함으로써 코드 베이스를 훨씬 더 깨끗하고 효율적으로 만들 수 있습니다. VBA는 같은 현대적 편의성을 가지고 있지 않을 수 있지만, 좋은 코드 디자인의 원칙은 보편적입니다. 다른 언어에서 오는 개발자들은 수동 프로세스를 지루하게 생각할 수 있지만, 처음부터 코드 품질을 향상시키는 데 시간을 투자하는 것의 혜택을 의심할 여지없이 높이 평가할 것입니다.

더 견고한 개발 환경에서 작업하거나 특히 정교한 프로젝트를 수행할 때, 더 강력한 리팩터링 도구를 제공하는 대안을 탐색하거나 VBA 프로젝트를 .NET 언어로 변환하여 Visual Studio가 광범위한 리팩터링 지원을 제공하는 것도 고려해볼 가치가 있습니다. 그럼에도 불구하고, VBA에서 리팩터링 원칙을 이해하고 적용하는 것은 깨끗하고 유지 보수 가능한 코드를 작성하는 중요성을 강조하는 귀중한 기술입니다.
