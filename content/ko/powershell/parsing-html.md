---
title:                "HTML 파싱"
html_title:           "Fish Shell: HTML 파싱"
simple_title:         "HTML 파싱"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/parsing-html.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하나요?

HTML 파싱이란 웹 페이지의 HTML 문서를 분석하고 이해하는 프로세스를 가리킵니다. 이를 통해 프로그래머들은 웹 페이지의 구조를 이해하거나 필요한 데이터를 추출하는 등 다양한 작업을 수행할 수 있습니다.

## 사용법:

PowerShell을 사용하여 HTML을 파싱하는 것은 간단합니다. 아래에 예시를 드릴게요.

```PowerShell
# 웹 페이지 가져오기
$page = Invoke-WebRequest -Uri "https://example.com"

# HTML 파싱하기
$html = New-Object -ComObject "HTMLFile"
$html.IHTMLDocument2_write($page.Content)

# 파싱된 요소 확인하기
$html.body.all.tags('a') | ForEach-Object { $_.innerText }
```

이 코드는 "https://example.com" 웹 페이지에서 모든 'a' 태그를 가져온 뒤, 그 내용을 출력합니다.

## 딥 다이브:

HTML 파싱은 웹의 복잡한 구조를 이해하고 처리하는 데 중요한 역할을 합니다. 이는 웹 크롤링 및 스크래핑, 웹 콘텐츠의 분석, 웹 페이지의 자동화된 디버깅 등에 필요한 기능입니다.

HTML 파싱에는 여러 방법이 있으며, 각각의 방법은 파싱할 웹 페이지의 구조 및 필요한 정보에 따라 적합하게 사용됩니다. 예를 들어, CSS 선택자를 사용하여 원하는 정보를 직접 추출할 수 있습니다.

PowerShell에서는 HTML 파서 -ComObject를 통해 파싱을 실행합니다. 이는 Windows 만에서 기본으로 이용할 수 있지만, PowerShell Core에서는 사용할 수 없습니다. 따라서 다른 플랫폼을 위해 HtmlAgilityPack 등의 라이브러리를 이용할 수 있습니다.

## 추가 정보:

HTML 파싱에 대해 더 알아보고 싶다면 아래 링크를 참조하세요.

1. [Microsoft PowerShell Documentation](https://docs.microsoft.com/en-us/powershell/)
2. [An introduction to HTML Parsing in PowerShell](https://4sysops.com/archives/an-introduction-to-html-parsing-in-powershell/)
3. [HtmlAgilityPack](https://html-agility-pack.net/)