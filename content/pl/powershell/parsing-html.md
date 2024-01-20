---
title:                "Analiza składniowa HTML"
html_title:           "Gleam: Analiza składniowa HTML"
simple_title:         "Analiza składniowa HTML"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/parsing-html.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Analiza składniowa HTML polega na przetwarzaniu kodu HTML do zrozumiałego formatu dla programów. Programiści robią to, aby ekstrahować dane, manipulować zawartością strony internetowej lub testować interoperacyjność.

## Jak to zrobić:

Można to zrobić z pomocą innej mocy PowerShell, jak `Invoke-WebRequest`. Uruchom poniższy kod, aby pobrać i przeanalizować stronę internetową.

```PowerShell
# Get the website content
$pageContent = Invoke-WebRequest -Uri "http://twojasuperstrona.pl"

# Parse the HTML content
$parsedContent = $pageContent.ParsedHtml

# Output the parsed HTML content
$parsedContent
```

## Głębsze zagłębienie:

Historia analizy składniowej HTML rozciąga się od czasów, gdy internet był jeszcze młody. Choć PowerShell stanowi efektywne narzędzie, istnieje wiele alternatyw takich jak BeautifulSoup dla Pythona czy jsdom dla JavaScript.

Szczegółowo, PowerShell korzysta z COM interfejsu HTML Document, który reprezentuje całą stronę HTML. Można go użyć do odnalezienia, dodania czy usunięcia elementów HTML.

## Zobacz również:

1. [Podstawowe nasłuchiwacze DOM](https://developer.mozilla.org/pl/docs/Web/API/Document_Object_Model)
2. [Parsowanie zawartości strony internetowej z PowerShell](https://4sysops.com/archives/parsing-html-webpages-with-powershell/)
3. [Dokumentacja `Invoke-WebRequest`](https://docs.microsoft.com/pl-pl/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7.1)