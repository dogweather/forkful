---
aliases:
- /fi/powershell/sending-an-http-request-with-basic-authentication/
date: 2024-01-20 18:02:44.035172-07:00
description: "HTTP-pyynt\xF6 perusautentikoinnilla tarkoittaa verkkopalvelimeen l\xE4\
  hetett\xE4v\xE4\xE4 pyynt\xF6\xE4, jossa k\xE4ytt\xE4j\xE4nimi ja salasana sis\xE4\
  ltyv\xE4t selkokielell\xE4 ja koodataan\u2026"
lastmod: 2024-02-18 23:09:07.852754
model: gpt-4-1106-preview
summary: "HTTP-pyynt\xF6 perusautentikoinnilla tarkoittaa verkkopalvelimeen l\xE4\
  hetett\xE4v\xE4\xE4 pyynt\xF6\xE4, jossa k\xE4ytt\xE4j\xE4nimi ja salasana sis\xE4\
  ltyv\xE4t selkokielell\xE4 ja koodataan\u2026"
title: "HTTP-pyynn\xF6n l\xE4hett\xE4minen perusautentikoinnilla"
---

{{< edit_this_page >}}

## What & Why? (Mitä ja Miksi?)

HTTP-pyyntö perusautentikoinnilla tarkoittaa verkkopalvelimeen lähetettävää pyyntöä, jossa käyttäjänimi ja salasana sisältyvät selkokielellä ja koodataan base64-muotoon. Ohjelmoijat tekevät tämän päästäkseen käsiksi suojattuihin resursseihin tai API:hin.

## How to: (Kuinka tehdään:)

```PowerShell
# Määritä käyttäjätunnus ja salasana
$kayttajatunnus = 'kayttaja123'
$salasana = 'salainenSana'

# Koodaa tunnukset base64-muotoon
$base64AuthInfo = [Convert]::ToBase64String([Text.Encoding]::ASCII.GetBytes("$kayttajatunnus:$salasana"))

# Luo HttpHeaders-objekti ja lisää Authorization-header
$headers = New-Object "System.Collections.Generic.Dictionary[[String],[String]]"
$headers.Add('Authorization', "Basic $base64AuthInfo")

# Lähetä HTTP GET -pyyntö
$response = Invoke-RestMethod -Uri 'https://esimerkki.com/api/data' -Method Get -Headers $headers

# Tulosta vastaus
$response
```

Vastaussisältö näkyy terminaalissa.

## Deep Dive (Syväsukellus)

Perusautentikointi on yksi vanhimmista HTTP-autentikointitavoista, helppo ymmärtää ja toteuttaa. Se on kuitenkin turvaton, koska base64-koodaus ei ole salausta, ja kun se lähetetään ilman SSL/TLS-salattua yhteyttä, käyttäjätiedot ovat alttiita urkinnalle.

Vaihtoehtoina ovat muut autentikointitavat, kuten OAuth tai token-pohjaiset järjestelmät, jotka tarjoavat paremman turvallisuuden. Ohjelmoijan on arvioitava perusautentikoinnin sopivuus käyttöön tapauskohtaisesti.

PowerShell-komennon `Invoke-RestMethod` avulla on suhteellisen yksinkertaista lähettää HTTP-pyyntöjä erilaisilla autentikointitavoilla. Kehittäjät voivat räätälöidä pyyntöjään tarpeen mukaan lisäämällä parametreja, kuten kehys (`Headers`), parametrit (`Body`) tai pyyntömetodin (`Method`).

## See Also (Katso Myös)

- [PowerShellin ohjeet Invoke-RestMethod:lle](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod)
- [Perusautentikoinnin ymmärtäminen](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#basic_authentication_scheme)
- [Base64-koodauksen selitys](https://en.wikipedia.org/wiki/Base64)
