---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:59:28.450876-07:00
description: "\uBC29\uBC95: \uB354 \uB2E8\uC21C\uD55C \uD504\uB85C\uADF8\uB798\uBC0D\
  \ \uD658\uACBD\uACFC \uB2EC\uB9AC, VBA\uB294 \uC8FC\uB85C Microsoft Office \uC560\
  \uD50C\uB9AC\uCF00\uC774\uC158 \uB0B4\uC5D0 \uB0B4\uC7A5\uB418\uB3C4\uB85D \uC124\
  \uACC4\uB418\uC5C8\uAE30 \uB54C\uBB38\uC5D0 \uC804\uD1B5\uC801\uC778 \uC758\uBBF8\
  \uC5D0\uC11C \uBA85\uB839\uC904 \uC778\uC218\uB97C \uC9C1\uC811 \uC77D\uC744 \uC218\
  \ \uC788\uB294 \uB0B4\uC7A5 \uAE30\uB2A5\uC774 \uC5C6\uC2B5\uB2C8\uB2E4. \uADF8\uB7EC\
  \uB098, \uC870\uAE08\uC758 \uCC3D\uC758\uC131\uC744 \uC0AC\uC6A9\uD558\uBA74 Windows\
  \ Script\u2026"
lastmod: '2024-03-13T22:44:55.006126-06:00'
model: gpt-4-0125-preview
summary: "\uB354 \uB2E8\uC21C\uD55C \uD504\uB85C\uADF8\uB798\uBC0D \uD658\uACBD\uACFC\
  \ \uB2EC\uB9AC, VBA\uB294 \uC8FC\uB85C Microsoft Office \uC560\uD50C\uB9AC\uCF00\
  \uC774\uC158 \uB0B4\uC5D0 \uB0B4\uC7A5\uB418\uB3C4\uB85D \uC124\uACC4\uB418\uC5C8\
  \uAE30 \uB54C\uBB38\uC5D0 \uC804\uD1B5\uC801\uC778 \uC758\uBBF8\uC5D0\uC11C \uBA85\
  \uB839\uC904 \uC778\uC218\uB97C \uC9C1\uC811 \uC77D\uC744 \uC218 \uC788\uB294 \uB0B4\
  \uC7A5 \uAE30\uB2A5\uC774 \uC5C6\uC2B5\uB2C8\uB2E4."
title: "\uBA85\uB839 \uC904 \uC778\uC218 \uC77D\uAE30"
weight: 23
---

## 방법:
더 단순한 프로그래밍 환경과 달리, VBA는 주로 Microsoft Office 애플리케이션 내에 내장되도록 설계되었기 때문에 전통적인 의미에서 명령줄 인수를 직접 읽을 수 있는 내장 기능이 없습니다. 그러나, 조금의 창의성을 사용하면 Windows Script Host(WSH)를 사용하거나 외부 API를 호출하여 비슷한 기능을 달성할 수 있습니다. 여기 WSH를 사용한 실용적인 우회 방법이 있습니다:

1. **VBA에 인수를 전달하는 VBScript 만들기:**

   먼저, VBA 애플리케이션(예: Excel 매크로)을 실행하고 명령줄 인수를 전달하는 VBScript 파일(*yourScript.vbs*)을 작성합니다:

```vb
Set objExcel = CreateObject("Excel.Application")
objExcel.Workbooks.Open "C:\YourMacroWorkbook.xlsm"
objExcel.Run "YourMacroName", WScript.Arguments.Item(0), WScript.Arguments.Item(1)
objExcel.Quit
```

2. **VBA에서 인수에 접근하기:**

   VBA 애플리케이션(*YourMacroWorkbook.xlsm*)에서 매개변수를 받아들일 수 있도록 매크로(*YourMacroName*)를 수정하거나 만듭니다:

```vb
Sub YourMacroName(arg1 As String, arg2 As String)
    MsgBox "Argument 1: " & arg1 & " Argument 2: " & arg2
End Sub
```

3. **스크립트 실행하기:**

   명령 줄에서 인수를 전달하며 VBScript를 실행합니다:

```shell
cscript yourScript.vbs "Hello" "World"
```

   이렇게 하면 "Hello"와 "World"라는 인수를 사용해 VBA 매크로가 실행되며, 메시지 박스에 이를 표시하게 됩니다.

## 심층 분석:
역사적 맥락에서, VBA는 독립 실행형 프로그래밍 환경이 아니라 Microsoft Office 애플리케이션의 기능을 확장하기 위해 고안되었습니다. 이러한 맥락에서, 명령 줄과의 직접적인 상호 작용은 그 주요 범위를 벗어나는 것이므로, 명령줄 인수를 읽기 위한 내장 지원이 없는 이유를 설명합니다.

위에서 설명한 방법은 효과적이지만, 외부 스크립팅을 활용하여 격차를 메우는 우회적인 해결책일 뿐입니다. 이 접근 방식은 매크로를 활성화하고 실행하기 위해 보안 설정을 낮출 수도 있으므로 복잡성과 잠재적 보안 문제를 도입할 수 있습니다.

명령줄 인수에 크게 의존하거나 Windows 운영 체제와 더 밀접한 통합이 필요한 작업의 경우, PowerShell이나 Python과 같은 다른 프로그래밍 언어가 더 강력하고 안전한 해결책을 제공할 수 있습니다. 이러한 대안들은 명령줄 인수에 대한 직접 지원을 제공하며, 외부 입력으로 그들의 동작을 동적으로 수정해야 하는 독립 실행형 애플리케이션 또는 스크립트에 더 적합합니다.
