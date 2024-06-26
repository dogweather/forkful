---
date: 2024-01-20 17:44:36.922313-07:00
description: "How to: Aloita. Avaa PowerShell. K\xE4yt\xE4 Invoke-WebRequest -komentoa."
lastmod: '2024-04-05T22:38:57.392865-06:00'
model: gpt-4-1106-preview
summary: "Aloita. Avaa PowerShell. K\xE4yt\xE4 Invoke-WebRequest -komentoa."
title: Verkkosivun lataaminen
weight: 42
---

## How to:
Aloita. Avaa PowerShell. Käytä Invoke-WebRequest -komentoa:

```PowerShell
$response = Invoke-WebRequest -Uri 'http://example.com'
$response.Content
```

Esimerkkitulostus näyttää ladatun verkkosivun HTML-koodin.

## Deep Dive
PowerShellissa sivun lataaminen liittyy Invoke-WebRequest -komennon käyttöön, joka esiteltiin versiossa 3.0. Vaihtoehtona voit käyttää System.Net.WebClient -luokkaa, mutta moderneissa skripteissä Invoke-WebRequest on suositumpi ratkaisu, koska se on suunniteltu nimenomaan PowerShellin kanssa yhteensopivaksi. Tämän komennon kautta saat paitsi sivun sisällön, myös pääsyyn otsakkeisiin, kekseihin ja HTTP-vastauksen statuksiin.

## See Also
- Microsoftin PowerShell-dokumentaatio: [Invoke-WebRequest](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest)
- Lisätietoja HTTP-protokollasta: [HTTP - MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/HTTP)
