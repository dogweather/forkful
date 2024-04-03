---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:05:34.838312-07:00
description: "CSV-tiedostojen (pilkulla erotetut arvot) k\xE4sittely k\xE4sitt\xE4\
  \xE4 lukemista tai kirjoittamista yksinkertaisiin tekstitiedostoihin, joissa tietokent\xE4\
  t ovat\u2026"
lastmod: '2024-03-13T22:44:56.424873-06:00'
model: gpt-4-0125-preview
summary: "CSV-tiedostojen (pilkulla erotetut arvot) k\xE4sittely k\xE4sitt\xE4\xE4\
  \ lukemista tai kirjoittamista yksinkertaisiin tekstitiedostoihin, joissa tietokent\xE4\
  t ovat erotettu pilkuilla."
title: "Ty\xF6skentely CSV:n kanssa"
weight: 37
---

## Kuinka:
Visual Basic for Applications (VBA) yksinkertaistaa CSV-tiedostojen käsittelyä sisäänrakennettujen funktioiden ja metodien kautta, jotka mahdollistavat sujuvan luvun ja kirjoittamisen näihin tiedostoihin. Alla on esimerkkejä, jotka havainnollistavat perustoimintoja CSV-tiedostojen kanssa.

### CSV-tiedoston lukeminen:
```basic
Sub ReadCSV()
    Dim filePath As String
    filePath = "C:\example.csv"
    
    Open filePath For Input As #1
    
    Do Until EOF(1)
        Dim line As String
        Line Input #1, line
        Dim dataFields() As String
        dataFields = Split(line, ",")
        
        'Käsittele dataFields-taulukkoa tarpeen mukaan
        Debug.Print Join(dataFields, ";") 'Esimerkkituloste, joka näyttää muutoksen pilkuista puolipisteisiin
    Loop
    
    Close #1
End Sub
```

### Kirjoittaminen CSV-tiedostoon:
```basic
Sub WriteCSV()
    Dim filePath As String
    filePath = "C:\output.csv"
    Dim dataToWrite As String
    dataToWrite = "ID,Name,Age" & vbCrLf & "1,John Doe,30" & vbCrLf & "2,Jane Doe,29"
    
    Open filePath For Output As #1
    Print #1, dataToWrite
    Close #1
End Sub
```

Esimerkkituloste `output.csv`-tiedostossa:
```
ID,Name,Age
1,John Doe,30
2,Jane Doe,29
```

## Syväsukellus
Historiallisesti CSV-tiedostot ovat olleet suoraviivainen menetelmä tallentaa taulukkomuotoista dataa tekstiformaatissa. Sen rakenteen yksinkertaisuus, jossa jokainen rivi vastaa yhtä datatietuetta ja jokainen kenttä tietueessa on erotettu pilkulla, on sekä CSV:n vahvuus että sen rajoitus. Formaatti ei natiivisti tue datatyyppejä, mikä tarkoittaa, että kaikki data tallennetaan merkkijonoina, ja datan oikeanlaiseen tyyppiin muuntamisen taakka lankeaa ohjelmoijalle.

Visual Basic for Applicationsissa CSV-tiedostojen käsittely tapahtuu pääasiassa perustiedosto-operaatioiden kautta, kuten aiemmissa esimerkeissä on näytetty. Ei ole suoraa CSV-jäsennystukea kuten moderneimmissa kielissä (esim. Pythonin csv-moduuli), joka tarjoaa enemmän hallintaa ja mukavuutta CSV-datan käsittelyssä.

Monimutkaisempien operaatioiden tai suurten CSV-tiedostojen käsittelyssä ohjelmoijat saattavat löytää parempia vaihtoehtoja VBA:n ulkopuolelta, kuten hyödyntämällä ulkoisia kirjastoja tai käyttämällä muita ohjelmointikieliä, jotka on varustettu kehittyneemmillä CSV-käsittelyominaisuuksilla. Kuitenkin yksinkertaisia tehtäviä varten, jotka liittyvät CSV-tiedostoihin, VBA:n suoraviivainen lähestymistapa on usein riittävä ja helppo toteuttaa, tarjoten nopean ratkaisun Excel-pohjaisiin sovelluksiin tai muihin Microsoft Office -ohjelmistojen automaatioon.
