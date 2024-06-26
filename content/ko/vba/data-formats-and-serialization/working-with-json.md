---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:05:40.408051-07:00
description: "\uC5B4\uB5BB\uAC8C \uC0AC\uC6A9\uD558\uB294\uAC00: VBA\uB294 \uAE30\uBCF8\
  \uC801\uC73C\uB85C JSON \uD30C\uC2F1 \uB610\uB294 \uC0DD\uC131\uC744 \uC9C0\uC6D0\
  \uD558\uC9C0 \uC54A\uC73C\uBBC0\uB85C, JSON \uBB38\uC790\uC5F4\uC744 \uD30C\uC2F1\
  \uD558\uACE0 JSON \uAC1D\uCCB4\uB97C \uC0DD\uC131\uD558\uAE30 \uC704\uD574 JScript\uC640\
  \ \uAC19\uC740 \uC2A4\uD06C\uB9BD\uD2B8 \uC5B8\uC5B4(\uC2A4\uD06C\uB9BD\uD2B8 \uCEE8\
  \uD2B8\uB864 \uAC1D\uCCB4\uB97C \uD1B5\uD574)\uB97C \uC0AC\uC6A9\uD560 \uAC83\uC785\
  \uB2C8\uB2E4. \uB2E4\uC74C\uC740 VBA\uC5D0\uC11C JSON \uBB38\uC790\uC5F4\uC744 \uD30C\
  \uC2F1\uD558\uB294\u2026"
lastmod: '2024-03-13T22:44:55.015784-06:00'
model: gpt-4-0125-preview
summary: "VBA\uB294 \uAE30\uBCF8\uC801\uC73C\uB85C JSON \uD30C\uC2F1 \uB610\uB294\
  \ \uC0DD\uC131\uC744 \uC9C0\uC6D0\uD558\uC9C0 \uC54A\uC73C\uBBC0\uB85C, JSON \uBB38\
  \uC790\uC5F4\uC744 \uD30C\uC2F1\uD558\uACE0 JSON \uAC1D\uCCB4\uB97C \uC0DD\uC131\
  \uD558\uAE30 \uC704\uD574 JScript\uC640 \uAC19\uC740 \uC2A4\uD06C\uB9BD\uD2B8 \uC5B8\
  \uC5B4(\uC2A4\uD06C\uB9BD\uD2B8 \uCEE8\uD2B8\uB864 \uAC1D\uCCB4\uB97C \uD1B5\uD574\
  )\uB97C \uC0AC\uC6A9\uD560 \uAC83\uC785\uB2C8\uB2E4."
title: "JSON\uACFC \uD568\uAED8 \uC77C\uD558\uAE30"
weight: 38
---

## 어떻게 사용하는가:
VBA는 기본적으로 JSON 파싱 또는 생성을 지원하지 않으므로, JSON 문자열을 파싱하고 JSON 객체를 생성하기 위해 JScript와 같은 스크립트 언어(스크립트 컨트롤 객체를 통해)를 사용할 것입니다. 다음은 VBA에서 JSON 문자열을 파싱하는 방법입니다:

```basic
Function ParseJSON(ByVal jsonString As String) As Object
    Dim scriptControl As Object
    Set scriptControl = CreateObject("MSScriptControl.ScriptControl")
    scriptControl.Language = "JScript"
    
    scriptControl.Eval "var obj = (" & jsonString & ")"
    Set ParseJSON = scriptControl.CodeObject.obj
End Function

Sub DemoParseJSON()
    Dim jsonString As String
    jsonString = "{""name"":""John"", ""age"":30, ""city"":""New York""}"
    
    Dim parsed As Object
    Set parsed = ParseJSON(jsonString)
    
    MsgBox "이름: " & parsed.name & ", 나이: " & parsed.age & ", 도시: " & parsed.city
End Sub
```

JSON을 생성하기 위해, 유사한 방식을 사용할 수 있으며, 연결을 통해 JSON 문자열을 구성할 수 있습니다:

```basic
Function GenerateJSON(name As String, age As Integer, city As String) As String
    GenerateJSON = "{""name"":""" & name & """, ""age"":" & age & ", ""city"":""" & city & """}"
End Function

Sub DemoGenerateJSON()
    Dim jsonString As String
    jsonString = GenerateJSON("Jane", 28, "Los Angeles")
    
    MsgBox jsonString
End Sub
```

## 심층 분석
위에서 보여준 접근 방식은 JavaScript 엔진에 작업을 외주하는 것과 본질적으로 동일하게, ScriptControl을 사용하여 JSON을 처리합니다. 이는 창의적인 해결 방안이지만, VBA 컨텍스트에서 JSON을 다루기 위한 가장 효율적이거나 현대적인 방법은 아닙니다. 더 복잡한 애플리케이션에서는 이 방법이 번거롭게 되고, 성능 오버헤드나 보안 문제를 도입할 수 있습니다. 왜냐하면 ScriptControl은 호스트 컴퓨터에 대한 전체 접근 권한을 가진 환경에서 실행되기 때문입니다.

다른 프로그래밍 환경들, 예를 들어 Python이나 JavaScript는 JSON을 위한 내장 지원을 제공하여, 광범위한 JSON 조작이 필요한 애플리케이션에 더 적합합니다. 이 언어들은 파싱과 생성뿐만 아니라 JSON 데이터의 조회와 포맷팅을 용이하게 하는 포괄적인 라이브러리를 제공합니다.

VBA에서 이러한 한계에도 불구하고, 웹 기반 데이터 교환과 설정 파일이 주로 JSON 형식으로 이루어지는 세계에서 JSON을 다루는 방법을 이해하는 것은 필수적입니다. VBA 프로그래머들이 이러한 기술을 마스터함으로써, 웹 API와 연동하거나, 설정 파일을 해석하거나, 심지어 단순한 웹 애플리케이션을 구축하는 기회가 열립니다. 그러나 프로젝트가 복잡성이 증가하거나 높은 성능을 요구할 때, 개발자는 더 JSON 친화적인 프로그래밍 환경을 활용하는 것을 고려할 수 있습니다.
