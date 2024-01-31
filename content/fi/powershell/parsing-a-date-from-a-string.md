---
title:                "Merkkijonosta päivämäärän jäsentäminen"
date:                  2024-01-20T15:37:59.441410-07:00
html_title:           "Bash: Merkkijonosta päivämäärän jäsentäminen"
simple_title:         "Merkkijonosta päivämäärän jäsentäminen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Muunnetaan päivämäärät merkkijonoista ymmärrettävään muotoon. Taklataan käyttäjän syötteitä, tiedostojen logimerkintöjä ja päivämäärämuotojen epäjohdonmukaisuutta. Se on välttämätöntä tiedonhallinnassa ja automaatiossa.

## How to: (Kuinka tehdä:)
```PowerShell
# Yksinkertainen päivämäärän jäsentäminen
$pvmMerkkijono = "25.03.2023"
$pvmObjekti = [datetime]::ParseExact($pvmMerkkijono, "dd.MM.yyyy", $null)
Write-Output $pvmObjekti

# Tulostaa: 25. maaliskuuta 2023 0.00.00

# Monimuotoista syötettä käsittelevä jäsentäminen
$useitaMuotoja = @("25-03-2023", "2023/03/25", "March 25, 2023")
$kulttuuri = [System.Globalization.CultureInfo]::InvariantCulture
foreach ($muoto in $useitaMuotoja) {
    $pvmObjekti = [datetime]::Parse($muoto, $kulttuuri)
    Write-Output $pvmObjekti
}

# Tulostaa:
# 25. maaliskuuta 2023 0.00.00
# 25. maaliskuuta 2023 0.00.00
# 25. maaliskuuta 2023 0.00.00
```

## Deep Dive (Syväluotaus)
Päivämäärien jäsentäminen merkkijonosta on ollut tarpeen päivämäärätietojen alkukausista lähtien, kun eri formaatteja käytettiin eri järjestelmissä. Historiallisesti se on ollut yksi yleisimmistä mutta haastavimmista ohjelmoinnin tehtävistä. TypeScript- ja JavaScript-kehittäjät käyttävät kirjastoja kuten Moment.js, kun taas Pythonissa on `datetime`-moduuli. PowerShellissä `[datetime]`-tyyppimuunnos ja `ParseExact`-metodit ovat kaksi tapaa tehdä töitä päivämäärien kanssa. Täsmällinen parsinta vaatii tiedon datan formaatista, kun taas enemmän jouston salliva `Parse` hoitaa useita formaatteja. Kulttuurin määrittäminen on tärkeää, sillä päivämääräformaatti voi vaihdella alueittain.

## See Also (Katso Myös)
- .NET:n dokumentaatio `DateTime`-luokasta: [DateTime Struct (System)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-6.0)
- Lisätietoa kulttuurikohtaisista muotoiluista: [CultureInfo Class (System.Globalization)](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo?view=net-6.0)
- Microsoftin PowerShell-käsikirja: [PowerShell Documentation](https://docs.microsoft.com/en-us/powershell/)
