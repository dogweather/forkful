---
title:                "테스트 작성하기"
aliases:
- ko/vba/writing-tests.md
date:                  2024-02-01T22:08:43.133098-07:00
model:                 gpt-4-0125-preview
simple_title:         "테스트 작성하기"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/vba/writing-tests.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

프로그래밍에서 테스트를 작성한다는 것은 특정 절차를 생성하여 코드 세그먼트의 기능성과 성능을 검증하는 것을 말하며, 다양한 조건에서 예상대로 작동하는지를 보장합니다. 프로그래머들은 이를 통해 버그를 조기에 발견하고 코드 품질을 향상시키며, 미래의 코드 유지보수와 향상을 용이하게 합니다.

## 어떻게:

Visual Basic for Applications(VBA)는 Python이나 JavaScript와 같은 언어에서 사용할 수 있는 테스트 프레임워크를 내장하고 있지 않지만, 여전히 코드의 무결성을 확인하기 위해 간단한 테스트 절차를 구현할 수 있습니다. 다음은 예를 들어 설명한 것입니다:

VBA에 두 숫자를 더하는 함수가 있다고 가정해 보겠습니다:

```basic
Function AddNumbers(x As Integer, y As Integer) As Integer
    AddNumbers = x + y
End Function
```

이 함수를 테스트하기 위해서는 출력값이 예상 결과와 일치하는지 검증하는 다른 절차를 작성할 수 있습니다:

```basic
Sub TestAddNumbers()
    Dim result As Integer
    result = AddNumbers(5, 10)
    If result = 15 Then
        MsgBox "테스트 통과!", vbInformation
    Else
        MsgBox "테스트 실패. 15를 기대했지만 " & result & "를 받았습니다.", vbCritical
    End If
End Sub
```

`TestAddNumbers`를 실행하면 함수의 출력 결과를 기반으로 테스트가 통과했는지 실패했는지를 나타내는 메시지 상자가 표시됩니다. 이것은 단순화된 시나리오이지만, 루프를 적용하거나, 다양한 입력값을 사용하거나, 여러 함수에 대한 테스트를 하는 것과 같이 더 복잡한 테스트를 구축할 수 있습니다.

## 심층 분석

여기서 보여준 VBA에서의 테스트 작성 방법은 수동이며 다른 프로그래밍 환경에서 사용할 수 있는 더 정교한 테스트 프레임워크의 기능, 예를 들면 자동 테스트 실행, 설정/철수 절차, 테스트 결과의 통합 보고 등을 갖추고 있지 않습니다. 단위 테스트 프레임워크와 테스트 주도 개발(TDD)의 더 넓은 채택 이전에는 설명된 것과 유사한 수동 테스트 절차가 일반적이었습니다. 이 방법은 간단하고 작은 프로젝트나 학습 목적에는 효과적일 수 있지만, 더 큰 프로젝트나 팀에는 확장성이 없거나 효율적이지 않습니다.

보다 풍부한 개발 도구 세트를 지원하는 환경에서 프로그래머들은 종종 .NET 애플리케이션을 위한 NUnit이나 Java 애플리케이션을 위한 JUnit과 같은 프레임워크로 전환합니다. 이 프레임워크들은 테스트 결과를 어설션하는 것부터 목 객체 설정, 코드 커버리지 측정에 이르기까지 체계적으로 테스트를 작성하고 실행하기 위한 종합적인 도구를 제공합니다.

더 진보된 테스트 기능을 찾고 있는 VBA 개발자들에게 가장 가까운 대안은 외부 도구를 활용하거나 다른 프로그래밍 환경과 통합하는 것일 수 있습니다. 일부 개발자들은 Excel과 함께 VBA를 사용하여 테스트 시나리오와 결과를 수동으로 기록합니다. 전용 테스트 프레임워크를 사용하는 것만큼 편리하거나 자동화되지는 않지만, 이러한 방법들은 복잡하거나 중요한 애플리케이션에서 VBA 솔루션의 신뢰성을 유지하는 데 도움을 줄 수 있습니다.
