---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:57:33.590645-07:00
description: "\uBC29\uBC95: VBA\uC5D0\uC11C\uB294 `Microsoft HTML Object Library`\uB97C\
  \ \uC0AC\uC6A9\uD558\uC5EC HTML\uC744 \uD30C\uC2F1\uD560 \uC218 \uC788\uC2B5\uB2C8\
  \uB2E4. VBA \uD3B8\uC9D1\uAE30\uC5D0\uC11C \uB3C4\uAD6C > \uCC38\uC870\uB85C \uC774\
  \uB3D9\uD558\uC5EC `Microsoft HTML Object Library`\uB97C \uCCB4\uD06C\uD568\uC73C\
  \uB85C\uC368 \uC774 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uC5D0 \uB300\uD55C \uCC38\uC870\
  \uB97C \uCD94\uAC00\uD569\uB2C8\uB2E4.\u2026"
lastmod: '2024-03-13T22:44:54.977498-06:00'
model: gpt-4-0125-preview
summary: "VBA\uC5D0\uC11C\uB294 `Microsoft HTML Object Library`\uB97C \uC0AC\uC6A9\
  \uD558\uC5EC HTML\uC744 \uD30C\uC2F1\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "HTML \uBD84\uC11D\uD558\uAE30"
weight: 43
---

## 방법:
VBA에서는 `Microsoft HTML Object Library`를 사용하여 HTML을 파싱할 수 있습니다. VBA 편집기에서 도구 > 참조로 이동하여 `Microsoft HTML Object Library`를 체크함으로써 이 라이브러리에 대한 참조를 추가합니다. 이렇게 하면 HTML 문서를 탐색하고 조작하기 위한 클래스들에 접근할 수 있습니다.

여기 파일에서 HTML 문서를 로드하고 모든 링크(앵커 태그)를 추출하는 간단한 예가 있습니다:

```vb
Sub ParseHTML()
    Dim htmlDoc As MSHTML.HTMLDocument
    Dim htmlElement As MSHTML.IHTMLElement
    Dim htmlElements As MSHTML.IHTMLElementCollection
    Dim htmlFile As String
    Dim fileContent As String
    
    ' 파일로부터 HTML 내용을 로드
    htmlFile = "C:\path\to\your\file.html"
    Open htmlFile For Input As #1
    fileContent = Input$(LOF(1), 1)
    Close #1
    
    ' HTML 문서 초기화
    Set htmlDoc = New MSHTML.HTMLDocument
    htmlDoc.body.innerHTML = fileContent
    
    ' 모든 앵커 태그 가져오기
    Set htmlElements = htmlDoc.getElementsByTagName("a")

    ' 모든 앵커 요소를 순회하며 href 속성을 출력
    For Each htmlElement In htmlElements
        Debug.Print htmlElement.getAttribute("href")
    Next htmlElement
End Sub
```

이 스크립트는 HTML 파일의 내용을 읽고, `HTMLDocument` 객체로 로드한 후, 모든 앵커 요소(`<a>` 태그)를 검색하고, 각각에 대해 `href` 속성을 즉시 창에 출력하면서 반복합니다.

## 심층 분석:
역사적으로, VBA에서 HTML을 파싱하는 것은 현대 웹 스크래핑 및 문서 처리 기술에 대한 직접적인 지원 부족으로 인해 다소 번거로운 작업이었습니다. Microsoft HTML Object Library는 강력하긴 하지만, 다소 오래되었으며 신기술보다 현대 웹 표준을 원활하게 다루지 못할 수 있습니다.

복잡한 HTML 파싱 및 웹 스크래핑 작업의 경우, Beautiful Soup 또는 Scrapy와 같은 라이브러리를 갖춘 Python과 같은 대체 도구 및 언어가 종종 권장됩니다. 이러한 현대 도구들은 더 큰 유연성, 더 나은 성능을 제공하며, 현재 웹 표준과 더 잘 맞습니다. 그러나 Microsoft Office 생태계 내에서 작업할 때, Microsoft HTML Object Library와 함께 사용하는 VBA는 여전히 귀중한 기술입니다. 이는 Excel 및 Access와 같은 애플리케이션과 원활하게 통합되어 HTML 콘텐츠를 직접 조작할 수 있는 방법을 제공함으로써, 친숙한 VBA 환경 밖으로 나가지 않고도 기본 HTML 문서 처리를 포함한 작업을 수행할 수 있는 간단한 방법을 제공합니다.
