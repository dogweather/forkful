---
title:                "Å dekode html"
html_title:           "PowerShell: Å dekode html"
simple_title:         "Å dekode html"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/parsing-html.md"
---

{{< edit_this_page >}}

Hva & Hvorfor?

Parsing av HTML er en prosess der man ekstraherer informasjon fra en HTML-kode for å få tilgang til innholdet i en nettside. Dette gjøres ofte av programmerere for å automatisere oppgaver, hente data eller analysere informasjon fra nettsider.

Hvordan:

I PowerShell kan man bruke Invoke-WebRequest cmdlet for å laste ned en nettside og så bruke Select-XML cmdlet for å parse gjennom HTML-koden og finne ønsket informasjon.

Eksempel:

```PowerShell
$url = "https://www.example.com/"
$response = Invoke-WebRequest $url
$html = [xml]$response.Content
$title = Select-XML -Xml $html -XPath "//title" | Select-Object -ExpandProperty Node | Select-Object -ExpandProperty InnerText
```

I dette eksempelet har vi brukt Invoke-WebRequest til å laste ned nettsiden til variabelen $response. Deretter har vi konvertert innholdet til HTML ved å bruke [xml] typecast. Vi har deretter brukt Select-XML og XPath for å finne tittelen på siden, og lagret den i variabelen $title.

Dypdykk:

Parsing av HTML er et viktig verktøy for å automatisere og effektivisere oppgaver for programmerere. Det finnes også alternative måter å parse HTML i PowerShell ved å bruke tredjepartsmoduler som HtmlAgilityPack eller AngleSharp.

Implementeringen av parsing i PowerShell er basert på XML-teknologi og kan følge vanlige HTML-regler som å angi tagger og attributter i XPath og bruke selectors som i CSS.

Se også:

- Microsofts dokumentasjon om Invoke-WebRequest og Select-XML cmdlets: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7
- Mer informasjon om XPath: https://www.w3schools.com/xml/xpath_intro.asp
- HtmlAgilityPack dokumentasjon: https://github.com/zzzprojects/html-agility-pack
- AngleSharp dokumentasjon: https://anglesharp.github.io/