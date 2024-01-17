---
title:                "Satunnaislukujen luominen."
html_title:           "PowerShell: Satunnaislukujen luominen."
simple_title:         "Satunnaislukujen luominen."
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Satunnaislukujen generointi on keino luoda ohjelmassa arvoja satunnaisesti. Monet ohjelmoijat käyttävät tätä muun muassa pelien, salasanojen ja arvontojen luomisessa.

## Kuinka tehdä?

Generoidaan satunnainen kokonaisluku väliltä 1-10:

```PowerShell
Get-Random -Minimum 1 -Maximum 10
```

Tulostaa esimerkiksi: 7

Kaikkiin parametreihin ei tarvitse antaa arvoja, jolloin oletusarvot otetaan käyttöön. Esimerkiksi kokonaislukujen sijaan voi generoida myös merkkijonoja:

```PowerShell
Get-Random -InputObject "HEAD", "TAIL"
```

Tulostaa esimerkiksi: TAIL

## Syväsukellus

Satunnaislukujen generointi on ollut osa tietojenkäsittelyä jo pitkään ja sillä on monia erilaisia käyttötarkoituksia. Ennen tietokoneiden yleistymistä satunnaislukuja luotiin fyysisiä prosesseja, kuten nopanheittoa tai arpajaisia, hyödyntäen. Nykyään tietokoneet tarjoavat erilaisia algoritmeja satunnaislukujen generointiin, joista yksi on käytössä myös PowerShellissa.

## Katso myös

[Lisätietoa satunnaislukujen generoinnista PowerShellissa](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-random?view=powershell-5.1)