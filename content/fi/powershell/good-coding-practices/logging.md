---
date: 2024-01-26 01:07:52.374532-07:00
description: "Lokit ovat k\xE4yt\xE4nn\xF6ss\xE4 murupolkuja koodisi l\xE4pi - ne\
  \ kertovat, mit\xE4 skriptillesi tapahtuu, kun se on \"villiss\xE4 luonnossa\" suorituksessa.\
  \ Ohjelmoijat\u2026"
lastmod: 2024-02-19 22:05:15.684836
model: gpt-4-1106-preview
summary: "Lokit ovat k\xE4yt\xE4nn\xF6ss\xE4 murupolkuja koodisi l\xE4pi - ne kertovat,\
  \ mit\xE4 skriptillesi tapahtuu, kun se on \"villiss\xE4 luonnossa\" suorituksessa.\
  \ Ohjelmoijat\u2026"
title: Lokitus
---

{{< edit_this_page >}}

## Mikä ja miksi?
Lokit ovat käytännössä murupolkuja koodisi läpi - ne kertovat, mitä skriptillesi tapahtuu, kun se on "villissä luonnossa" suorituksessa. Ohjelmoijat lokittavat vianetsintää, sovelluskäyttäytymisen seuraamista, suorituskyvyn monitorointia ja mahdollisten ongelmien tarkkailua varten.

## Kuinka:
Tässä vinkkejä peruslokituksen sisällyttämiseksi skripteihisi:

```PowerShell
# Yksinkertaisen lokiviestin luominen
Write-Host "Info: Aloittaa skriptiprosessin."

# Kirjoittaminen tiedostoon
"Info: Tämä on lokitettu viesti." | Out-File -Append myLog.log

# Sisäänrakennetun cmdletin käyttäminen yksityiskohtaisempaan lokitukseen
Start-Transcript -Path "./detailedLog.log"
Write-Output "Varoitus: Jotakin ei ole aivan kohdallaan."
# ... skriptisi tekee asioita
Stop-Transcript

# detailedLog.log-tuloste
******************************
Windows PowerShellin käsikirjoituksen aloitus
Aloitusaika: 20230324112347
Käyttäjätunnus : PShellGuru@example.com
RunAs-käyttäjä: PShellGuru@example.com
Konfiguroinnin nimi: 
Kone  : PS-DEVBOX (Microsoft Windows NT 10.0.17763.0)
Isäntäsovellus: C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe
Prosessi ID: 2024
PS-versio: 7.1.2
```

Nyt lokeissasi on toiminta kuvattuna siitä, mitä koodillesi on tapahtunut.

## Syväsukellus:
Historiallisesti lokitus on ollut yhtä vanha kuin ohjelmointikin. Se on kuin kapteenin loki, mutta ohjelmistolle. Aikaisemmin se on saattanut olla printtereitä tai teletyyppilaitteita; nykyään on kyse tiedostoista ja kehittyneistä lokien hallintajärjestelmistä.

Kun olet syvällä PowerShellin juoksuhaudoissa, `Write-Host` on nopea ja likainen tapa, mutta se vain heittää tekstiä konsoliin, mikä ei ole hienoa arkistoinnin kannalta. `Out-File` tarjoaa yksinkertaisen tavan ohjata tekstiä tiedostoon, mutta todellista mehua varten haluat käyttää `Start-Transcript` ja `Stop-Transcript`, jotka lokittavat kaiken - syötteet, tulosteet, koko paketin.

Vaihtoehtoja? Toki, jos pyörität yritystason asioita, saatat katsoa Windowsin tapahtumalokia tai käyttää ohjelmistoja kuten Logstash, mutta arkipäiväisen skriptisi kanssa pysy PowerShellin työkalujen parissa. Toteutuksesta muista lokittaa fiksusti - liian vähän ja se on hyödytöntä, liian paljon ja se on vain valkoista kohinaa.

## Katso myös:
Tutustu näihin saadaksesi kattavan käsityksen lokien hallinnasta PowerShellissa:
