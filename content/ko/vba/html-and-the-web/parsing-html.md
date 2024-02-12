---
title:                "HTML 분석하기"
date:                  2024-02-01T21:57:33.590645-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML 분석하기"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/vba/parsing-html.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇이며 왜?

Visual Basic for Applications(VBA)에서 HTML을 파싱하는 것은 HTML 문서에서 특정 정보를 추출하는 작업을 말합니다. 프로그래머들은 웹 페이지에서 데이터를 읽고 처리하는 과정을 자동화하기 위해 이를 수행합니다. 예를 들어, 웹사이트 콘텐츠를 스크래핑하거나, Microsoft Excel 또는 Access와 같이 VBA를 지원하는 애플리케이션 내에서 폼 제출 및 데이터 검색을 자동화하는 등의 작업이 해당됩니다.

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
