---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:58.983787-07:00
description: "\uBC29\uBC95: PowerShell\uC740 \uAE30\uBCF8\uC801\uC73C\uB85C \uC804\
  \uC6A9 HTML \uD30C\uC11C\uB97C \uAC00\uC9C0\uACE0 \uC788\uC9C0 \uC54A\uC9C0\uB9CC\
  , `Invoke-WebRequest` cmdlet\uC744 \uC0AC\uC6A9\uD558\uC5EC HTML \uCF58\uD150\uCE20\
  \uC5D0 \uC811\uADFC\uD558\uACE0 \uD30C\uC2F1\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4\
  . \uBCF4\uB2E4 \uBCF5\uC7A1\uD55C \uD30C\uC2F1\uACFC \uC870\uC791\uC744 \uC704\uD574\
  \uC11C\uB294 \uC778\uAE30 \uC788\uB294 .NET \uB77C\uC774\uBE0C\uB7EC\uB9AC\uC778\
  \u2026"
lastmod: '2024-03-13T22:44:55.543695-06:00'
model: gpt-4-0125-preview
summary: "PowerShell\uC740 \uAE30\uBCF8\uC801\uC73C\uB85C \uC804\uC6A9 HTML \uD30C\
  \uC11C\uB97C \uAC00\uC9C0\uACE0 \uC788\uC9C0 \uC54A\uC9C0\uB9CC, `Invoke-WebRequest`\
  \ cmdlet\uC744 \uC0AC\uC6A9\uD558\uC5EC HTML \uCF58\uD150\uCE20\uC5D0 \uC811\uADFC\
  \uD558\uACE0 \uD30C\uC2F1\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "HTML \uD30C\uC2F1"
weight: 43
---

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
