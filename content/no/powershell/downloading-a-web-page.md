---
title:                "Laste ned en nettside"
html_title:           "Elixir: Laste ned en nettside"
simple_title:         "Laste ned en nettside"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

---

## Hva & Hvorfor? 

Å laste ned en webside betyr å hente data fra en webserver og lagre den lokalt på maskinen din. Programmerere gjør dette for å manipulere data, analysere innhold, eller for offline tilgang.

---

## Hvordan: 

La oss se på et grunnleggende eksempel på hvordan du laster ned en webside ved hjelp av PowerShell:

```PowerShell
$url = "http://eksempel.com"
$response = Invoke-WebRequest -Uri $url
$response.Content | Out-File -FilePath .\webside.html -Encoding utf8
```

Dette vil laste ned websiden og lagre den som `webside.html` i din nåværende katalog. Innholdet i filen vil være HTML-koden på websiden.

---

## Dypdykk: 

Historisk sett har det alltid vært mulig å laste ned websider, men hver programmeringsspråk gjør det på sin egen måte. Med PowerShell 3.0 og nyere, kan du bruke `Invoke-WebRequest` cmdlet for å laste ned websider. Men det er alternativer, som cURL, som kan være mer velkjent for de som kommer fra Unix/Linux bakgrunn.

Når det gjelder implementeringsdetaljer, oppretter `Invoke-WebRequest` cmdlet en GET-forespørsel til webserveren, og lagrer svaret i et HttpResponse objekt. Du kan da enkelt hente innholdet ved å bruke `$response.Content`.

---

## Se Også:


- [Microsoft Docs: Invoke-WebRequest](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7)
- [Stack Overflow: How to download a website with PowerShell](https://stackoverflow.com/questions/3629817/is-it-possible-to-download-using-the-windows-command-line)
- [PowerShell: Scripting guide to PowerShell](https://docs.microsoft.com/en-us/powershell/scripting/overview?view=powershell-7.1)