---
title:                "Virheiden käsittely"
date:                  2024-01-26T00:57:58.607513-07:00
model:                 gpt-4-1106-preview
simple_title:         "Virheiden käsittely"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/handling-errors.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Virheiden käsittely PowerShellissa tarkoittaa mahdollisten ongelmien ennakointia ja niiden sujuvaa hallintaa. Ohjelmoijat tekevät tämän estääkseen ohjelmien kaatumiset ja tarjoavat käyttäjille hyödyllisiä palautteita.

## Kuinka:
```PowerShell
# Perus Try-Catch poikkeusten käsittelyyn
try {
    # Koodi, joka voi laukaista virheen
    $result = 1 / 0
} catch {
    # Mitä tehdä jos virhe tapahtui
    Write-Host "Hups, virhe tapahtui: $_"
}

# Määritellyn virheviestin tulostaminen
try {
    Get-Item "olematontiedosto.txt" -ErrorAction Stop
} catch {
    Write-Host "Tiedostoa ei löytynyt."
}

# $Error-muuttujan käyttö viimeisimmän virheen tarkasteluun
```
## Syväsukellus
PowerShell on tullut pitkän matkan siitä, kun se tunnettiin Monadina. Virheenkäsittely on kehittynyt ajan myötä, ja se tarjoaa ominaisuuksia, jotka ovat verrattavissa muihin ohjelmointikieliin. Esimerkiksi `try-catch-finally` syntaksi on peräisin kielistä kuten C#. Ennen sitä skriptinkirjoittajat nojautuivat vahvasti ehtojen tarkistamiseen ja `$Error`-automaattiseen muuttujaan.

PowerShellissa on myös kaksi päätyyppiä virheitä: terminoivat ja ei-terminoivat virheet. Terminoivat virheet pysäyttävät skriptin ellei niitä napatu `try-catch`-lohkossa, kun taas ei-terminoivat virheet eivät pysäytä, ellet määritä `-ErrorAction Stop`. Tämä ero on ratkaisevan tärkeä, sillä se antaa tarkkaan hallita virheiden käsittelyä, päättäen pidetäänkö virhe tarpeeksi merkittävänä koko skriptin pysäyttämiseksi vai voiko sen vain kirjata ylös ja ohittaa.

PowerShellin virheenkäsittely mahdollistaa myös `finally`-lohkon käytön, joka suoritetaan joka tapauksessa - olipa virhettä tai ei. Se on loistava siivoustehtäviin.

Kun olet syvällä skriptien kaivannossa, voit myös käsitellä tiettyjä poikkeustyyppejä, mikä antaa vieläkin hienosyisemmän hallinnan.

Toisaalta on vanhan koulukunnan `-ErrorVariable`-parametri, jolla voit kaapata virheet heittämättä poikkeusta. Ja `$?`-muuttuja kertoo, oliko viime toiminto onnistunut. Nämä ovat käteviä työkaluja, vaikkakin hieman vähemmän siistejä kuin kunnon `try-catch`.

## Katso Myös
- [about_Try_Catch_Finally](https://docs.microsoft.com/fi-fi/powershell/module/microsoft.powershell.core/about/about_try_catch_finally?view=powershell-7.2)
