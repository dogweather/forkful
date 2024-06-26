---
date: 2024-01-20 18:00:30.085656-07:00
description: "Hur man g\xF6r: F\xF6rr anv\xE4nde vi ofta `Invoke-WebRequest` f\xF6\
  r HTTP-interaktioner, men `Invoke-RestMethod` f\xF6renklar JSON-hantering och deserialisering.\u2026"
lastmod: '2024-04-05T22:50:52.428405-06:00'
model: gpt-4-1106-preview
summary: "F\xF6rr anv\xE4nde vi ofta `Invoke-WebRequest` f\xF6r HTTP-interaktioner,\
  \ men `Invoke-RestMethod` f\xF6renklar JSON-hantering och deserialisering."
title: "Skicka en http-f\xF6rfr\xE5gan"
weight: 44
---

## Hur man gör:
```PowerShell
# Skicka en GET-begäran för att hämta innehåll från en webbsida
$response = Invoke-RestMethod -Uri 'https://api.example.com/data' -Method Get
Write-Output $response

# Skicka en POST-begäran med JSON-innehåll
$body = @{
    name = 'Förnamn Efternamn'
    email = 'fornamn@example.com'
}
$json = $body | ConvertTo-Json
$response = Invoke-RestMethod -Uri 'https://api.example.com/submit' -Method Post -Body $json -ContentType 'application/json'
Write-Output $response
```
Sample utdata:
```json
{
  "userId": 1,
  "id": 1,
  "title": "Att skicka HTTP-begäran med PowerShell",
  "completed": false
}
```

## Fördjupning:
Förr använde vi ofta `Invoke-WebRequest` för HTTP-interaktioner, men `Invoke-RestMethod` förenklar JSON-hantering och deserialisering. Alternativ till PowerShell är cURL eller programmeringsspråk som Python och JavaScript med ramverk som `requests` eller `axios`. Implementationsdetaljer kan inkludera hantering av header-autentisering och felhantering när servern svarar med ogiltiga koder.

## Se Även:
- [PowerShell Documentation for Invoke-RestMethod](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod)
- [W3Schools HTTP Methods](https://www.w3schools.com/tags/ref_httpmethods.asp)
- [MDN Web Docs - HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP)
