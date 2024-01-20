---
title:                "Kahden päivämäärän vertaaminen"
html_title:           "Bash: Kahden päivämäärän vertaaminen"
simple_title:         "Kahden päivämäärän vertaaminen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Päivämäärien vertailu tarkoittaa kahta tai useampaa päivämäärää verrattavaksi niiden välisten erojen määrittelemiseksi. Se on hyödyllistä aikapohjaisten toimintojen hallinnassa, kuten tehtävien keston seurannassa tai ajanjaksojen laskemisessa.

## Miten:

Käytä `DateTime`-luokkaa päivämäärien vertailemiseen PowerShellissa. Tässä on yksinkertainen esimerkki siitä, miten se toimii:

```PowerShell
# Luodaan kaksi päivämäärää
$date1 = Get-Date -Year 2021 -Month 6 -Day 12
$date2 = Get-Date -Year 2021 -Month 7 -Day 12

# Vertaillaan päivämääriä
if ($date1 -gt $date2) {
    Write-Output "Date1 on myöhemmin kuin Date2"
} elseif ($date1 -lt $date2) {
    Write-Output "Date1 on aikaisemmin kuin Date2"
} else {
    Write-Output "Date1 ja Date2 ovat samana päivänä"
}
```

Tämä tuottaa seuraavan tuloksen:

```PowerShell
"Date1 on aikaisemmin kuin Date2"
```

## Syvempi sukellus:

Päivämäärien vertailu on ollut osa ohjelmointia sen alkuvaiheista lähtien. Aiemmin tämä oli monimutkainen prosessi, mutta modernit ohjelmointikielet, kuten PowerShell, tekevät siitä nopean ja yksinkertaisen toiminnon.

Vaihtoehtoisissa ohjelmointikielessä, kuten Pythonissa tai JavaScriptissa, päivämäärien vertailu toimii samalla logiikalla, mutta syntaksi vaihtelee.

PowerShellissa `DateTime`-luokka hoitaa kaiken työn. Se huomioi sekä aikavyöhykkeet että karkausvuodet tehdessään vertailuja, joten koodaajan ei tarvitse huolehtia näistä yksityiskohdista.

## Katsele myös:

Vastaavat resurssit, jotka käsittelevät päivämäärien vertailua ja niiden käyttöä PowerShellin kanssa:

- PowerShellin `DateInterval`-luokka: [Linkki](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.compare?view=net-5.0)