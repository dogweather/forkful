---
title:                "HTML 파싱"
date:                  2024-01-20T15:33:31.694024-07:00
html_title:           "Arduino: HTML 파싱"
simple_title:         "HTML 파싱"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
HTML 파싱은 HTML 마크업에서 데이터를 추출하는 과정입니다. 프로그래머들은 웹 페이지의 내용을 자동으로 읽고 가공하기 위해 이를 수행합니다.

## How to: (어떻게 하나요?)
PowerShell에서는 HTML 파싱을 다양한 방법으로 수행할 수 있습니다. 가장 간단한 방법 중 하나는 `Invoke-WebRequest`로 HTML을 가져온 다음 HTML Agility Pack 같은 라이브러리를 사용하여 파싱하는 것입니다.

```PowerShell
# 필요한 라이브러리를 설치합니다.
Install-Package HtmlAgilityPack

# 웹 페이지를 가져옵니다.
$response = Invoke-WebRequest -Uri 'http://example.com'

# HTML Agility Pack을 이용하여 파싱합니다.
$html = New-Object HtmlAgilityPack.HtmlDocument
$html.LoadHtml($response.Content)

# 특정 요소를 선택하여 출력합니다.
$node = $html.DocumentNode.SelectSingleNode('//h1')
$node.InnerText
```

출력 예시:

```
웹 페이지의 제목
```

## Deep Dive: (심층 분석)
HTML 파싱은 웹의 초창기부터 중요한 작업이었습니다. PowerShell이 등장하기 전에는 Perl이나 Python 같은 다른 언어가 이 작업을 주로 담당했습니다. PowerShell에서는 내장된 `Invoke-WebRequest`와 `Invoke-RestMethod` 같은 명령어를 제공하여 웹 콘텐츠 작업을 간소화했습니다.

하지만 HTML 파싱은 복잡할 수 있으며, HTML이 항상 정규화되지는 않기 때문에 강인한 파서가 필요합니다. HTML Agility Pack은 .NET 기반의 라이브러리로서 이런 복잡성을 다루고, XPath나 CSS 선택자를 사용해 원하는 데이터를 찾을 수 있게 해줍니다.

대안으로는 `AngleSharp`, `CsQuery`와 같은 다른 .NET 라이브러리들이 있으며, 이들도 비슷한 방식으로 작동합니다. PowerShell 스크립트를 사용하여 이 라이브러리들을 활용할 수 있고, 그로 인해 파싱 과정이 매우 유연해집니다.

## See Also: (관련 자료)
- HTML Agility Pack 공식 문서: [https://html-agility-pack.net/](https://html-agility-pack.net/)
- PowerShell 문서: [https://docs.microsoft.com/en-us/powershell/](https://docs.microsoft.com/en-us/powershell/)
- AngleSharp GitHub 페이지: [https://github.com/AngleSharp/AngleSharp](https://github.com/AngleSharp/AngleSharp)
- CsQuery GitHub 페이지: [https://github.com/jamietre/CsQuery](https://github.com/jamietre/CsQuery)