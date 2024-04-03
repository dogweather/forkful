---
date: 2024-01-20 17:44:41.231167-07:00
description: "How to: Anv\xE4nd `Invoke-WebRequest` f\xF6r att ladda ner en webbsida."
lastmod: '2024-03-13T22:44:38.124829-06:00'
model: gpt-4-1106-preview
summary: "Anv\xE4nd `Invoke-WebRequest` f\xF6r att ladda ner en webbsida."
title: "H\xE4mta en webbsida"
weight: 42
---

## How to:
Använd `Invoke-WebRequest` för att ladda ner en webbsida.
```PowerShell
$response = Invoke-WebRequest -Uri 'https://example.com'
$response.Content | Out-File 'example_page.html'
```
Exempelutmatning: skapar filen `example_page.html` med webbsidans innehåll.

## Deep Dive
`Invoke-WebRequest` kom i PowerShell 3.0 och används för att interagera med webb-API:er och sidor. Alternativ som `curl` eller `wget` finns i andra system men är inte inbyggda i PowerShell till skillnad från `Invoke-WebRequest`. Ett detalj att notera är att `Invoke-WebRequest` hanterar cookies och sessionsinformation automatiskt, vilket är bra för komplexa webbsidor.

## See Also
- [PowerShell Documentation for Invoke-WebRequest](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest)
- [Using PowerShell to automate tasks: Web scraping](https://www.varonis.com/blog/powershell-web-scraping/)
