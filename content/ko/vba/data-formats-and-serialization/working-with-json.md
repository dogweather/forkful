---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:05:40.408051-07:00
description: "JSON(JavaScript Object Notation)\uC740 \uC0AC\uB78C\uC774 \uC77D\uACE0\
  \ \uC4F0\uAE30 \uC26C\uC6B0\uBA70, \uAE30\uACC4\uAC00 \uD30C\uC2F1\uD558\uACE0 \uC0DD\
  \uC131\uD558\uAE30\uC5D0 \uD3B8\uD55C \uACBD\uB7C9 \uB370\uC774\uD130 \uAD50\uD658\
  \ \uD615\uC2DD\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC11C\
  \uBC84\uC640 \uC6F9 \uC560\uD50C\uB9AC\uCF00\uC774\uC158 \uAC04\uC758 \uB370\uC774\
  \uD130\uB97C \uC804\uC1A1\uD558\uAC70\uB098 \uB2E4\uC591\uD55C \uD504\uB85C\uADF8\
  \uB798\uBC0D \uD658\uACBD\uC5D0\uC11C, \uC608\uB97C \uB4E4\uC5B4 Visual Basic\u2026"
lastmod: 2024-02-19 22:05:13.923217
model: gpt-4-0125-preview
summary: "JSON(JavaScript Object Notation)\uC740 \uC0AC\uB78C\uC774 \uC77D\uACE0 \uC4F0\
  \uAE30 \uC26C\uC6B0\uBA70, \uAE30\uACC4\uAC00 \uD30C\uC2F1\uD558\uACE0 \uC0DD\uC131\
  \uD558\uAE30\uC5D0 \uD3B8\uD55C \uACBD\uB7C9 \uB370\uC774\uD130 \uAD50\uD658 \uD615\
  \uC2DD\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC11C\uBC84\
  \uC640 \uC6F9 \uC560\uD50C\uB9AC\uCF00\uC774\uC158 \uAC04\uC758 \uB370\uC774\uD130\
  \uB97C \uC804\uC1A1\uD558\uAC70\uB098 \uB2E4\uC591\uD55C \uD504\uB85C\uADF8\uB798\
  \uBC0D \uD658\uACBD\uC5D0\uC11C, \uC608\uB97C \uB4E4\uC5B4 Visual Basic\u2026"
title: "JSON\uACFC \uD568\uAED8 \uC77C\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가?

JSON(JavaScript Object Notation)은 사람이 읽고 쓰기 쉬우며, 기계가 파싱하고 생성하기에 편한 경량 데이터 교환 형식입니다. 프로그래머들은 서버와 웹 애플리케이션 간의 데이터를 전송하거나 다양한 프로그래밍 환경에서, 예를 들어 Visual Basic for Applications(VBA)에서 정보를 구조화되고 접근 가능한 방식으로 저장하기 위해 JSON을 사용합니다.

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
