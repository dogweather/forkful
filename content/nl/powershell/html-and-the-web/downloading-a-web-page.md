---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:25.969461-07:00
description: 'Hoe: Hier is de magische formule om een webpagina te fetchen met PowerShell.
  We zullen `Invoke-WebRequest` gebruiken.'
lastmod: '2024-03-13T22:44:51.027837-06:00'
model: gpt-4-0125-preview
summary: Hier is de magische formule om een webpagina te fetchen met PowerShell.
title: Een webpagina downloaden
weight: 42
---

## Hoe:
Hier is de magische formule om een webpagina te fetchen met PowerShell. We zullen `Invoke-WebRequest` gebruiken.

```PowerShell
# Pak de inhoud van example.com
$response = Invoke-WebRequest -Uri "http://example.com"

# Dit is wat je hebt gekregen
$response.Content
```

Voorbeelduitvoer:

```PowerShell
<!doctype html>
<html>
<head>
    <title>Voorbeeld Domein</title>
    ...
    <!-- en zo verder -->
</head>
...
</html>
```

Je bent misschien alleen na tekst, geen HTML-tags. Laten we dat doen:

```PowerShell
# Alleen de tekst, alsjeblieft
$response.ParsedHtml.body.innerText
```

## Diepgaande Duik
Er was eens een tijd waarin PowerShell niet beschikte over de coole `Invoke-WebRequest` cmdlet. Programmeurs zouden de .NET `System.Net.WebClient` klasse gebruiken of hun toevlucht nemen tot externe tools. Nu, is het allemaal ingebouwd, waardoor taken voor ons allemaal vereenvoudigd worden.

`Invoke-WebRequest` biedt meer dan alleen inhoud. Headers, status en sessie-informatie – het is er allemaal. Als je met API's speelt, zul je `Invoke-RestMethod` geweldig vinden als een gefocust alternatief.

Onder de motorkap vertrouwen deze cmdlets op de zware .NET HttpClient klasse, betrouwbaarheid en uitgebreide functionaliteit verpakkend.

En, als je ongeduldig wordt terwijl je wacht op het downloaden van die webpagina, ondersteunt `Invoke-WebRequest` ook asynchrone operaties. Echter, dat is een onderwerp voor een andere dag.

## Zie Ook
- De [Invoke-WebRequest documentatie](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest)
- Meer over [Invoke-RestMethod voor API-interacties](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod)
- Een [PowerShell GitHub-repository](https://github.com/PowerShell/PowerShell) voor de nieuwsgierige programmeurs die graag onder de motorkap kijken.
