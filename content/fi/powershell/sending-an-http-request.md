---
title:                "Lähettämällä http-pyyntö"
html_title:           "PowerShell: Lähettämällä http-pyyntö"
simple_title:         "Lähettämällä http-pyyntö"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
HTTP-pyyntöjen lähettäminen on tapa kommunikoida tietokoneiden välillä internetissä. Ohjelmoijat tekevät tätä voidakseen hakea tietoja muista tietokoneista, viestiä palvelimien kanssa tai tehdä muita verkkotoimintoja. 

## Miten:
Esimerkkejä koodista ja esimerkkilähtöön sisältyvät koodiesimerkit käyttäen ```PowerShell...``` -lohkoja.

Esimerkki 1: Lähetä HTTP-GET-pyyntö ja tulosta vastaus:

```PowerShell
$request = Invoke-WebRequest -Uri https://example.com/api/data -Method Get
$request.Content
```
Esimerkki 2: Lähetä HTTP-POST-pyyntö JSON-dataan ja käsittele vastaus:

```PowerShell
$data = @{ 
    name = "Johanna"
    age = 28
    job = "Software Developer"
} | ConvertTo-Json

$request = Invoke-WebRequest -Uri https://example.com/api/users -Method Post -Body $data -ContentType "application/json"
$response = ConvertFrom-Json $request.Content
Write-Output "Uusi käyttäjä luotu: $response.name, $response.job, $response.age vuotta"
```

## Syvemmälle:
HTTP on protokolla, jota käytetään datan lähettämiseen ja vastaanottamiseen internetissä. Sitä voidaan käyttää monilla eri tavoilla, kuten lähettämään pyyntöjä sähköpostipalvelimelle tai lataamaan kuvia verkkosivuilta. Muita tapoja lähettää HTTP-pyyntöjä PowerShellilla on Triple-H-toiminto tai .NET Frameworkin käyttö.

## Katso lisää:
Lisää tietoa HTTP-protokollasta ja sen käytöstä löydät täältä:
- PowerShellin dokumentaatio: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7.1
- HTTP-pyyntöjen lähettämisen vaihtoehdot: https://www.powershellmagazine.com/2014/08/01/playing-with-http-requests-in-powershell/
- HTTP-pyyntöjen toteuttaminen .NET Frameworkin avulla: https://docs.microsoft.com/en-us/dotnet/api/system.net.httpwebrequest?view=net-5.0