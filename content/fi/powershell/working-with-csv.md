---
title:                "Työskentely CSV:n kanssa"
aliases:
- fi/powershell/working-with-csv.md
date:                  2024-02-03T19:20:59.839026-07:00
model:                 gpt-4-0125-preview
simple_title:         "Työskentely CSV:n kanssa"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?

CSV-tiedostojen (pilkulla erotetut arvot) käsittely on yleinen tehtävä datan hallitsemiseksi ja manipuloimiseksi rakenteellisessa, taulukkomuodossa. Ohjelmoijat suorittavat usein tätä operaatiota tuodakseen, viedäkseen tai käsitelläkseen dataa tehokkaasti erilaisissa sovelluksissa, kuten datan analysointi, raportointi tai jopa web-sovellusten tehostaminen.

## Kuinka:

### CSV-tiedoston lukeminen

CSV-tiedostosta lukemiseksi käytä `Import-Csv` cmdlet-komentoa. Tämä cmdlet lukee tiedoston ja muuntaa sen mukautetuiksi PowerShell-objekteiksi jokaista riviä varten.

```powershell
# CSV-tiedoston tuominen
$data = Import-Csv -Path "C:\Data\users.csv"
# Sisällön näyttäminen
$data
```

**Esimerkkituloste:**

```
Name    Age    City
----    ---    ----
John    23     New York
Doe     29     Los Angeles
```

### Kirjoittaminen CSV-tiedostoon

Vastaavasti, kun halutaan kirjoittaa dataa CSV-tiedostoon, käytetään `Export-Csv` cmdlet-komentoa. Tämä cmdlet ottaa syötteeksi objekteja ja muuntaa ne CSV-muotoon.

```powershell
# Vietävän objektin luominen
$users = @(
    [PSCustomObject]@{Name='John'; Age='23'; City='New York'},
    [PSCustomObject]@{Name='Doe'; Age='29'; City='Los Angeles'}
)

# Vieminen CSV-tiedostoon
$users | Export-Csv -Path "C:\Data\new_users.csv" -NoTypeInformation
```

Suorituksen jälkeen luodaan tiedosto nimeltä `new_users.csv` annetuilla tiedoilla.

### CSV-sisällön suodattaminen ja muokkaaminen

CSV-tiedoston datan suodattamiseksi tai muokkaamiseksi käytä PowerShellin objektien käsittelykykyjä. Esimerkiksi, valitaksesi vain käyttäjät, jotka ovat tietyn iän yläpuolella ja tietystä kaupungista:

```powershell
# Datan tuominen ja suodattaminen
$filteredData = Import-Csv -Path "C:\Data\users.csv" | Where-Object {
    $_.Age -gt 25 -and $_.City -eq 'Los Angeles'
}

# Suodatetun datan näyttäminen
$filteredData
```

**Esimerkkituloste:**

```
Name    Age    City
----    ---    ----
Doe     29     Los Angeles
```

### Kolmannen osapuolen kirjastojen käyttäminen

Vaikka PowerShellin natiivit cmdlet-komennot yleensä riittävät yleisiin tehtäviin, monimutkaisemmat operaatiot saattavat hyötyä kolmannen osapuolen kirjastoista tai työkaluista. Kuitenkin, tavallisille CSV-käsittelyille, kuten lukemiselle, kirjoittamiselle, suodattamiselle tai lajittelulle, PowerShellin sisäänrakennetut cmdlet-komennot, kuten `Import-Csv` ja `Export-Csv`, tarjoavat yleensä vankan toiminnallisuuden ilman tarvetta ylimääräisille kirjastoille.
