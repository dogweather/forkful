---
title:                "HTML 파싱"
aliases: - /ko/powershell/parsing-html.md
date:                  2024-02-03T19:12:58.983787-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML 파싱"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?
PowerShell에서 HTML 파싱은 특정 데이터를 추출하거나 웹 관련 작업을 자동화하기 위해 HTML 콘텐츠를 분석하는 것을 의미합니다. 프로그래머들은 웹 페이지와 상호작용하거나 웹 콘텐츠를 스크레이핑하거나 웹 브라우저 없이 폼 제출이나 다른 웹 상호작용을 자동화하기 위해 이 작업을 수행합니다.

## 방법:

PowerShell은 기본적으로 전용 HTML 파서를 가지고 있지 않지만, `Invoke-WebRequest` cmdlet을 사용하여 HTML 콘텐츠에 접근하고 파싱할 수 있습니다. 보다 복잡한 파싱과 조작을 위해서는 인기 있는 .NET 라이브러리인 HtmlAgilityPack을 활용할 수 있습니다.

### `Invoke-WebRequest` 사용하기:

```powershell
# 웹페이지에서 제목을 가져오는 간단한 예
$response = Invoke-WebRequest -Uri 'http://example.com'
# ParsedHtml 속성을 사용하여 DOM 요소에 접근
$title = $response.ParsedHtml.title
Write-Output $title
```

샘플 출력:

```
Example Domain
```

### HtmlAgilityPack 사용하기:

먼저, HtmlAgilityPack을 설치해야 합니다. NuGet 패키지 관리자를 통해 설치할 수 있습니다:

```powershell
Install-Package HtmlAgilityPack -ProviderName NuGet
```

그런 다음, PowerShell에서 HTML을 파싱하기 위해 사용할 수 있습니다:

```powershell
# HtmlAgilityPack 어셈블리 로드
Add-Type -Path "path\to\HtmlAgilityPack.dll"

# HtmlDocument 객체 생성
$doc = New-Object HtmlAgilityPack.HtmlDocument

# 파일이나 웹 요청에서 HTML 로드
$htmlContent = (Invoke-WebRequest -Uri "http://example.com").Content
$doc.LoadHtml($htmlContent)

# XPath나 다른 쿼리 메소드를 사용하여 요소 추출
$node = $doc.DocumentNode.SelectSingleNode("//h1")

if ($node -ne $null) {
    Write-Output $node.InnerText
}
```

샘플 출력:

```
Welcome to Example.com!
```

이 예제에서, `Invoke-WebRequest`는 간단한 작업에 가장 적합하며, 반면 HtmlAgilityPack은 복잡한 HTML 파싱 및 조작을 위한 훨씬 더 풍부한 기능 세트를 제공합니다.
