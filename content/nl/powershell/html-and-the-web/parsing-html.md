---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:36.383935-07:00
description: 'Hoe: Laten we wat gegevens van een webpagina halen. We gebruiken Invoke-WebRequest
  en dan filteren we uit wat we nodig hebben.'
lastmod: '2024-03-13T22:44:51.026906-06:00'
model: gpt-4-0125-preview
summary: Laten we wat gegevens van een webpagina halen.
title: HTML Parsen
weight: 43
---

## Hoe:
Laten we wat gegevens van een webpagina halen. We gebruiken Invoke-WebRequest en dan filteren we uit wat we nodig hebben.

```PowerShell
# Haal de pagina-inhoud op
$response = Invoke-WebRequest -Uri "http://example.com"

# Parseer de HTML-inhoud
$parsedHtml = $response.ParsedHtml

# Gegevens extraheren
# Stel we willen alle hyperlinkteksten
$links = $parsedHtml.getElementsByTagName('a') | ForEach-Object { $_.innerText }
$links
```

Voorbeelduitvoer:

```
Home
Over Ons
Diensten
Contact
```

## Diepgaande Duik
Historisch gezien kon het parsen van HTML in PowerShell lastig zijn. Je had de keuze om regex te gebruiken (berucht problematisch voor HTML), COM-objecten met Internet Explorer, of externe bibliotheken. Nu vereenvoudigt PowerShell's Invoke-WebRequest cmdlet het proces, door te integreren met de Internet Explorer-engine om HTML te parsen — al is het een beetje traag en omslachtig.

Er bestaan alternatieven zoals de HtmlAgilityPack-bibliotheek, die veel robuuster is en beter afgestemd voor het parsen van HTML. Het vereist extra setup maar loont met flexibiliteit en prestatie.

Wat betreft de implementatie, let op dat de aanpak van PowerShell niet altijd accuraat is voor dynamische content gevuld door JavaScript. Om dynamische content te hanteren, heb je mogelijk browserautomatiseringstools zoals Selenium nodig.

## Zie Ook
- [HtmlAgilityPack op GitHub](https://github.com/zzzprojects/html-agility-pack)
- [Selenium met PowerShell](https://github.com/adamdriscoll/selenium-powershell)
