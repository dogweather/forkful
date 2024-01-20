---
title:                "Päivämäärän jäsentäminen merkkijonosta"
html_title:           "Bash: Päivämäärän jäsentäminen merkkijonosta"
simple_title:         "Päivämäärän jäsentäminen merkkijonosta"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

---

## Mitä ja Miksi?

Parsing-päivämäärä merkkijonosta tarkoittaa, että jäsennämme päivämäärän merkkijonojen muodossa ja muutamme sen päivämääräobjektiksi, jota voimme käsitellä ohjelmoinnissa. Sitä tarvitaan, kun teemme ohjelmia, jotka lukevat tai tallentavat päivämäärärajoituksia merkkijonona.

---

## Kuinka tehdä:

```PowerShell
# Luodaan merkkijono, joka sisältää päivämäärän
$päivämääräMerkkijono = "21.12.2022"

# Muunnetaan merkkijono päivämääräksi
$päivämäärä = [DateTime]::ParseExact($päivämääräMerkkijono, 'dd.MM.yyyy', $null)

# Tulostetaan päivämäärä
Write-Output $päivämäärä
```
Esimerkkisuorituksen tulostus on: `21. joulukuuta 2022 0:00:00`

---

## Syvällistä:

1. **Historiallinen konteksti**:
Parsing-päivämäärä merkkijonoista on yleinen tehtävä, joka on ollut olemassa ohjelmoinnin alkuaikojen jälkeen. PowerShellin syntaksi suorittaa tämän käyttämällä `ParseExact` -metodia.

2. **Vaihtoehtoja**:
Voit myös käyttää `DateTime.TryParse` -metodia provosoidaksesi virheitä, jos merkkijono ei ole suoritettavissa päivämääränä.

```PowerShell
$päivämääräMerkkijono = "21.12.2022"
$onnistuuko = [DateTime]::TryParseExact($päivämääräMerkkijono, 'dd.MM.yyyy', $null, [System.Globalization.DateTimeStyles]::None, [ref]$päivämäärä)

if($onnistuuko) {
    Write-Output $päivämäärä
} else {
    Write-Output "Ei voitu muuntaa päivämääräksi"
}
```

3. **Rakentamistiedot**: 
`ParseExact` -metodi ottaa 3 parametria: (1) merkkijonon, joka jäsennetään, (2) formaatilla esitetyt päivämäärän muotoilusäännöt, ja (3) kulttuuri-informaatio, jota käytetään jäsennyksessä. Jos tämä arvo on $null, käytetään nykyistä kulttuuria.

---

## Katso myös:

2. [DateTime.TryParse-metodi](https://docs.microsoft.com/fi-fi/dotnet/api/system.datetime.tryparse?view=net-5.0)
3. [DateTime.ParseExact-metodi](https://docs.microsoft.com/fi-fi/dotnet/api/system.datetime.parseexact?view=net-5.0)

---