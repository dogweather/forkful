---
title:                "Analisi dell'html"
html_title:           "PowerShell: Analisi dell'html"
simple_title:         "Analisi dell'html"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/parsing-html.md"
---

{{< edit_this_page >}}

## Cosa e Perché?
Il parsing HTML è il processo di analisi di un documento HTML per estrarre informazioni specifiche da esso. I programmatori lo fanno per ottenere dati strutturati da pagine web e automatizzare alcune attività, come il web scraping.

## Come:
```PowerShell
# Esempio 1: Parsare un documento HTML e trovare tutti i link sullo stesso
$pagina = Invoke-WebRequest "www.esempio.com"
$pagina.Links | select href

# Output:
Href
----
www.esempio.com/pagina1.html
www.esempio.com/pagina2.html
www.esempio.com/pagina3.html

# Esempio 2: Estrarre tutti i paragrafi da un documento HTML
$pagina = Invoke-WebRequest "www.esempio.com"
$pagina.ParsedHtml.getElementsByTagName("p") | select innerText

# Output:
InnerText
--------
Questo è un paragrafo.
Questo è un altro paragrafo.
Anche questo è un paragrafo.
```

## Approfondimento:
Il parsing HTML è un processo essenziale per la creazione di applicazioni web e per la raccolta di dati da internet. È possibile utilizzare strumenti come il modulo Invoke-WebRequest di PowerShell o librerie esterne come HtmlAgilityPack per realizzare il parsing. Alcune alternative al parsing HTML includono l'utilizzo di API o la lettura diretta del codice sorgente delle pagine web.

## Vedi Anche:
- [Documentazione di Invoke-WebRequest](https://docs.microsoft.com/en-us/powershell/module/Microsoft.PowerShell.Utility/Invoke-WebRequest?view=powershell-7)
- [HtmlAgilityPack su GitHub](https://github.com/zzzprojects/html-agility-pack)
- [Web Scraping con PowerShell](https://www.red-gate.com/simple-talk/sysadmin/powershell/web-scraping-with-powershell/)