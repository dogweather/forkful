---
date: 2024-01-20 18:00:44.528628-07:00
description: "How to: (Kuinka tehd\xE4\xE4n:) Esimerkki: Hae JSON-datan GET-pyynn\xF6\
  ll\xE4."
lastmod: '2024-04-05T21:53:58.353979-06:00'
model: gpt-4-1106-preview
summary: "(Kuinka tehd\xE4\xE4n:) Esimerkki."
title: "HTTP-pyynn\xF6n l\xE4hett\xE4minen"
weight: 44
---

## How to: (Kuinka tehdään:)
Esimerkki: Hae JSON-datan GET-pyynnöllä.
```PowerShell
$response = Invoke-RestMethod -Uri 'https://api.example.com/data' -Method Get
$response
```
Esimerkkituloste:
```PowerShell
name   : PowerShell
type   : CoolnessOverload
status : Success
```

Esimerkki: Lähetä JSON-datan POST-pyynnöllä.
```PowerShell
$body = @{
  name = 'PowerShell'
  type = 'CoolnessOverload'
}
$json = $body | ConvertTo-Json
$response = Invoke-RestMethod -Uri 'https://api.example.com/submit' -Method Post -Body $json -ContentType 'application/json'
$response
```
Esimerkkituloste:
```PowerShell
result  : Success
message : Data received
```

## Deep Dive (Syväsukellus)
HTTP-pyyntöjä on hyödynnetty 1990-luvun alusta, REST API:ien tullessa suosituiksi 2000-luvulla. PowerShellissä `Invoke-RestMethod` on yleinen tapa lähettää HTTP-pyyntöjä, ja se tukee erilaisia HTTP-metodeja, kuten GET, POST, PUT, ja DELETE.

Vaihtoehtoja:
- `Invoke-WebRequest`: Käytetään, kun tarvitaan yksityiskohtaista vastauksen hallintaa, kuten HTTP-vastauskoodeja tai otsikoita.
- cURL tai wget: Eri käyttöjärjestelmissä suosittu työkalu HTTP-pyyntöihin komentoriviltä.

Toteutustiedot:
- Älä unohda asettaa oikea sisällön tyyppi (`ContentType`), erityisesti POST-pyyntöjä lähettäessä.
- Autentikaatio ja otsikoiden määritys tapahtuvat käyttämällä `-Headers` parametria.
- Asynkroniset pyynnöt ja suorituskyky on mahdollista hallita `-AsJob` parametrin avulla.

## See Also (Lisätietoja)
- [Invoke-RestMethod documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod)
- [About REST APIs](https://www.redhat.com/en/topics/api/what-is-a-rest-api)
- [PowerShell scripting tutorial](https://docs.microsoft.com/en-us/powershell/scripting/overview)
- [HTTP Status Codes](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status)
