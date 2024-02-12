---
title:                "표준 오류에 쓰기"
aliases:
- /ko/vba/writing-to-standard-error/
date:                  2024-02-01T22:09:11.217438-07:00
model:                 gpt-4-0125-preview
simple_title:         "표준 오류에 쓰기"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/vba/writing-to-standard-error.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

Visual Basic for Applications(VBA)에서 표준 오류로의 기록은 표준 출력과 분리하여 오류 메시지나 진단 정보를 주로 콘솔이나 로그 파일로 전달하는 것을 포함합니다. 프로그래머들은 이러한 방법으로 정규 프로그램 출력과 오류 메시지를 분리함으로써 프로그램의 디버깅을 용이하게 하거나 주 출력을 혼란스럽게 하지 않고 사용자에게 문제를 알릴 수 있습니다.

## 방법:

VBA에서는 다른 프로그래밍 언어와 같이 표준 오류에 특별히 기록하는 직접적인 내장 함수가 없으므로, 개발 중 오류 출력을 위해 `Debug.Print`를 사용하거나, 생산 응용 프로그램을 위해 이러한 동작을 모방하는 사용자 정의 로깅 함수를 생성하는 것이 일반적인 해결책입니다. 아래는 이러한 함수를 구현하고 사용하는 방법의 예입니다:

```vb
Sub WriteToErrorLog(msg As String)
    ' 표준 오류에 기록하는 것을 모방하는 사용자 정의 함수
    ' 실제 배포에서는 별도의 로그 파일이나 전용 디버깅 창에 기록할 수 있습니다
    Open "ErrorLog.txt" For Append As #1 ' "ErrorLog.txt"를 원하는 로그 파일 경로로 변경하세요
    Print #1, "ERROR: " & msg
    Close #1
    Debug.Print "ERROR: " & msg ' IDE의 즉시 실행 창에도 출력하여 개발자가 디버깅할 수 있게 합니다
End Sub

Sub Demonstration()
    ' WriteToErrorLog 함수의 예제 사용
    WriteToErrorLog "귀하의 요청을 처리하는 동안 오류가 발생했습니다."
End Sub
```

"ErrorLog.txt"의 샘플 출력은 다음과 같을 수 있습니다:
```
ERROR: 귀하의 요청을 처리하는 동안 오류가 발생했습니다.
```

그리고 VBA IDE의 즉시 실행 창에서:
```
ERROR: 귀하의 요청을 처리하는 동안 오류가 발생했습니다.
```

## 심층 분석

Visual Basic for Applications은 Excel, Word 또는 Access와 같은 호스트 응용 프로그램과의 밀접한 통합 때문에 표준 오류에 쓰기를 위한 전용 메커니즘을 본질적으로 포함하지 않습니다. 이러한 호스트 응용 프로그램들은 전통적으로 콘솔 출력보다는 그래픽 사용자 인터페이스에 의존합니다. 이는 C나 Python과 같은 언어로 개발된 콘솔 기반 응용 프로그램에서 표준 출력 및 표준 오류 스트림이 기본 개념인 것과 눈에 띄는 차이입니다.

역사적으로, VBA의 초점은 항상 그것의 호스트 응용 프로그램의 문서 모델과 상호 작용하는 데 더 많았고 전통적인 응용 프로그램 로깅 메커니즘에는 덜 초점을 맞추었습니다. 따라서, 개발자들은 예제에서 본 것처럼 사용자 정의 로깅 솔루션을 구현하거나, 보다 고급 오류 처리 및 로깅 요구 사항에 대응하기 위해 Windows API 호출을 사용하는 경우가 많습니다.

제시된 접근 방식은 해결책을 제공하지만, 더 강력한 로깅 및 오류 처리를 원하는 개발자들은 보다 정교한 로깅을 할 수 있는 외부 시스템이나 라이브러리와의 통합을 탐색할 수 있습니다. 현대 개발에서 특히 디버깅 및 유지 관리에 초점을 맞추고 있기 때문에, 표준 및 오류 출력의 명확하고, 맥락적이며, 별도의 로깅의 중요성은 과대평가될 수 없으며 많은 사람들이 VBA의 기본 기능을 넘어서 해결책을 찾고 있습니다.
