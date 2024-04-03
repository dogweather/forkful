---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:04:45.658330-07:00
description: "\uBC29\uBC95: VBA\uC5D0\uC11C `Dictionary` \uAC1D\uCCB4\uB294 \uC5F0\
  \uAD00 \uBC30\uC5F4\uACFC \uC720\uC0AC\uD55C \uAE30\uB2A5\uC744 \uC81C\uACF5\uD569\
  \uB2C8\uB2E4. \uC0AC\uC6A9\uD558\uAE30 \uC704\uD574\uC11C\uB294 \uBA3C\uC800 Microsoft\
  \ Scripting Runtime\uC5D0 \uB300\uD55C \uCC38\uC870\uB97C \uCD94\uAC00\uD574\uC57C\
  \ \uD569\uB2C8\uB2E4: 1. VBA \uD3B8\uC9D1\uAE30\uC5D0\uC11C \uB3C4\uAD6C > \uCC38\
  \uC870\uB85C \uC774\uB3D9... 2. \"Microsoft\u2026"
lastmod: '2024-03-13T22:44:54.969128-06:00'
model: gpt-4-0125-preview
summary: "VBA\uC5D0\uC11C `Dictionary` \uAC1D\uCCB4\uB294 \uC5F0\uAD00 \uBC30\uC5F4\
  \uACFC \uC720\uC0AC\uD55C \uAE30\uB2A5\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4."
title: "\uC5F0\uAD00 \uBC30\uC5F4 \uC0AC\uC6A9\uD558\uAE30"
weight: 15
---

## 방법:
VBA에서 `Dictionary` 객체는 연관 배열과 유사한 기능을 제공합니다. 사용하기 위해서는 먼저 Microsoft Scripting Runtime에 대한 참조를 추가해야 합니다:

1. VBA 편집기에서 도구 > 참조로 이동...
2. "Microsoft Scripting Runtime"을 체크하고 확인을 클릭합니다.

`Dictionary`를 선언, 채우기 및 항목에 접근하는 방법은 다음과 같습니다:

```vb
Dim sampleDictionary As Dictionary
Set sampleDictionary = New Dictionary

' 항목 추가
sampleDictionary.Add Key:="Name", Item:="John Doe"
sampleDictionary.Add Key:="Age", Item:=29
sampleDictionary.Add Key:="Occupation", Item:="Engineer"

' 항목 접근
Debug.Print sampleDictionary.Item("Name")  ' 출력: John Doe
Debug.Print sampleDictionary.Item("Age")   ' 출력: 29

' 키 존재 여부 확인
If sampleDictionary.Exists("Occupation") Then
    Debug.Print "Occupation 키가 존재합니다"
End If

' 항목 제거
sampleDictionary.Remove("Occupation")

' 사전을 통해 반복
For Each Key In sampleDictionary.Keys
    Debug.Print Key & ": " & sampleDictionary.Item(Key)
Next Key
```

## 심층 분석
`Dictionary` 객체는 내부적으로 Windows Scripting Host의 구성 요소와 인터페이스를 합니다. 따라서, 늦게 바인딩된 COM 객체로서 과거에 VBA 기능을 확장하는 일반적인 방법이었습니다. VBA에서의 사용은 복잡한 데이터셋을 조작할 수 있는 능력을 크게 향상시키지만, 전통적인 배열이나 Excel 범위에서 볼 수 있는 엄격한 구조를 강요하지 않습니다.

한 가지 제한 사항은 `Dictionary`에 접근하기 위해 Microsoft Scripting Runtime에 대한 참조를 설정해야 한다는 점입니다. 이는 VBA 프로젝트의 분배를 복잡하게 만들 수 있습니다. `Dictionary`의 주요 기능 중 일부, 예를 들어 오류를 유발하지 않고 키의 존재를 쉽게 확인할 수 있는 기능과 같은 일부를 결여한 채 VBA 내에 존재하는 Collections와 같은 대체물이 있습니다.

더 최근의 프로그래밍 상황에서는 파이썬과 같은 언어가 외부 참조를 추가할 필요 없이 연관 배열(파이썬에서도 사전이라고 함)에 대한 내장 지원을 제공합니다. 이 내장 지원은 과정을 간소화하고 상자에서 바로 더 고급 기능을 제공합니다. 하지만 VBA의 범위 내 및 Microsoft Office 제품군에서 작업을 자동화하는 특정 애플리케이션을 위해, `Dictionary` 객체를 사용하는 것은 연관 배열과 유사한 데이터 구조에 대해 여전히 강력하고 관련성이 있는 방법입니다.
