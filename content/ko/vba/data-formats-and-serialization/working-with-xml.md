---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:07:10.155266-07:00
description: "\uC5B4\uB5BB\uAC8C: XML\uACFC \uC0C1\uD638 \uC791\uC6A9\uD558\uAE30\
  \ \uC2DC\uC791\uD558\uB824\uBA74, \uC77C\uBC18\uC801\uC73C\uB85C `MSXML2.DOMDocument`\
  \ \uAC1D\uCCB4\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4. \uC774 \uC778\uD130\uD398\uC774\
  \uC2A4\uB97C \uD1B5\uD574 XML \uBB38\uC11C\uB97C \uB85C\uB4DC, \uD30C\uC2F1\uD558\
  \uACE0 \uD0D0\uC0C9\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uC544\uB798\uB294 XML\
  \ \uD30C\uC77C\uC744 \uB85C\uB4DC\uD558\uACE0, \uAD6C\uC870\uB97C \uD0D0\uC0C9\uD558\
  \uBA70, \uC18D\uC131\uACFC \uD14D\uC2A4\uD2B8 \uB0B4\uC6A9\uC744 \uC77D\uB294 \uBC29\
  \uBC95\uC744\u2026"
lastmod: '2024-03-13T22:44:55.020849-06:00'
model: gpt-4-0125-preview
summary: "XML\uACFC \uC0C1\uD638 \uC791\uC6A9\uD558\uAE30 \uC2DC\uC791\uD558\uB824\
  \uBA74, \uC77C\uBC18\uC801\uC73C\uB85C `MSXML2.DOMDocument` \uAC1D\uCCB4\uB97C \uC0AC\
  \uC6A9\uD569\uB2C8\uB2E4."
title: "XML\uC744 \uD65C\uC6A9\uD55C \uC791\uC5C5\uD558\uAE30"
weight: 40
---

## 어떻게:
XML과 상호 작용하기 시작하려면, 일반적으로 `MSXML2.DOMDocument` 객체를 사용합니다. 이 인터페이스를 통해 XML 문서를 로드, 파싱하고 탐색할 수 있습니다. 아래는 XML 파일을 로드하고, 구조를 탐색하며, 속성과 텍스트 내용을 읽는 방법을 보여주는 간단한 예제입니다.

```basic
' 먼저, "Microsoft XML, v6.0"에 대한 참조를 도구 -> 참조를 통해 추가했는지 확인합니다
Dim xmlDoc As MSXML2.DOMDocument60
Set xmlDoc = New MSXML2.DOMDocument60
xmlDoc.async = False
xmlDoc.Load("C:\경로\파일.xml") ' XML 파일을 로드합니다

' XML이 성공적으로 로드되었는지 확인합니다
If xmlDoc.parseError.ErrorCode <> 0 Then
    MsgBox "XML 로드 오류:" & xmlDoc.parseError.reason
Else
    ' 요소 탐색 및 읽기
    Dim book As IXMLDOMNode
    Set book = xmlDoc.SelectSingleNode("//book/title") ' <book> 내 첫 번째 <title>을 찾기 위한 XPath
    MsgBox book.Text ' 제목 텍스트 보여주기
End If
```

위의 샘플 코드에서, 우리는 `MSXML2.DOMDocument60`의 인스턴스를 생성하고 XML 파일을 로드한 다음 오류를 확인합니다. 오류가 발견되지 않으면 XPath를 사용하여 특정 노드로 이동하여 그 텍스트 내용을 표시합니다.

## 심층 탐구:
VBA에서 XML 기능의 통합은 2000년대 초반 Office 응용 프로그램이 웹 데이터 및 서비스와 상호 작용할 필요가 커지기 시작했을 때로 거슬러 올라갑니다. `MSXML` 라이브러리, 또는 Microsoft XML Core 서비스는 시간이 지남에 따라 발전해왔으며, `MSXML2.DOMDocument60`는 개선된 성능 및 보안 기능으로 인해 사용하기에 권장되는 최신 버전 중 하나입니다.

강력함에도 불구하고, VBA의 XML 처리 기능은 Python의 XML.etree나 C#'s LINQ to XML과 같은 현대 프로그래밍 환경에 비해 덜 효율적이고 더 번거롭다고 여겨집니다. VBA의 내재적인 장황함과 수동으로 참조를 추가하고 관리해야 하는 요구는 빠른 개발을 저해할 수 있습니다. 또한, JSON 같은 더 가벼운 데이터 교환 형식의 출현으로 많은 프로그래머와 애플리케이션들이 XML에서 멀어지고 있으나, 상속 시스템이나 특정 기업 서비스와의 호환성이 필요한 경우에는 여전히 사용되고 있습니다.

그러나 Microsoft Office 자동화의 맥락에서 XML 문서를 파싱하거나 생성하는 작업이 필요한 경우, VBA의 XML 처리 기능을 활용하는 것은 여전히 실행 가능하고 때로는 필요한 접근 방식입니다. 이는 Office 응용 프로그램의 풍부한 기능 세트와 XML이 제공하는 구조화된 데이터 조작 능력 사이의 균형을 이루는 것입니다.
