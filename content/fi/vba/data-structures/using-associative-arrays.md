---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:04:36.615463-07:00
description: "Kuinka: VBA:ssa `Dictionary`-objekti tarjoaa toiminnallisuutta, joka\
  \ on samankaltaista kuin assosiatiiviset taulukot. Sinun on ensin lis\xE4tt\xE4\
  v\xE4 viittaus\u2026"
lastmod: '2024-03-13T22:44:56.392811-06:00'
model: gpt-4-0125-preview
summary: VBA:ssa `Dictionary`-objekti tarjoaa toiminnallisuutta, joka on samankaltaista
  kuin assosiatiiviset taulukot.
title: "Assosiatiivisten taulukoiden k\xE4ytt\xF6"
weight: 15
---

## Kuinka:
VBA:ssa `Dictionary`-objekti tarjoaa toiminnallisuutta, joka on samankaltaista kuin assosiatiiviset taulukot. Sinun on ensin lisättävä viittaus Microsoft Scripting Runtimeen sen käyttämiseksi:

1. VBA-editorissa, siirry Työkalut > Viitteet...
2. Valitse "Microsoft Scripting Runtime" ja klikkaa OK.

Näin voit julistaa, täyttää ja käyttää kohteita `Dictionary`ssa:

```vb
Dim sampleDictionary As Dictionary
Set sampleDictionary = New Dictionary

' Kohteiden lisääminen
sampleDictionary.Add Key:="Name", Item:="John Doe"
sampleDictionary.Add Key:="Age", Item:=29
sampleDictionary.Add Key:="Occupation", Item:="Engineer"

' Kohteiden käyttö
Debug.Print sampleDictionary.Item("Name")  ' Tuloste: John Doe
Debug.Print sampleDictionary.Item("Age")   ' Tuloste: 29

' Tarkistetaan onko avain olemassa
If sampleDictionary.Exists("Occupation") Then
    Debug.Print "Occupation Key Exists"
End If

' Kohteiden poistaminen
sampleDictionary.Remove("Occupation")

' Sanakirjan läpikäynti
For Each Key In sampleDictionary.Keys
    Debug.Print Key & ": " & sampleDictionary.Item(Key)
Next Key
```

## Syväsukellus
`Dictionary`-objekti toimii yhteistyössä Windows Scripting Hostin komponenttien kanssa. Näin ollen, se on myöhäisesti sidottu COM-objekti, mikä oli yleinen tapa laajentaa VBA:n toiminnallisuutta menneisyydessä. Sen käyttö VBA:ssa voi merkittävästi parantaa kielen kykyä käsitellä monimutkaisia datasettejä pakottamatta jäykkään rakenteeseen, kuten perinteisissä taulukoissa tai Excel-alueissa.

Yksi rajoitus, joka tulee pitää mielessä, on että `Dictionary`n käyttö vaatii viittauksen asettamisen Microsoft Scripting Runtimeen, mikä voi vaikeuttaa VBA-projektiesi jakelua. VBA:ssa on olemassa vaihtoehtoja, kuten Collections, mutta ne puuttuvat joitakin `Dictionary`n keskeisiä ominaisuuksia, kuten kykyä helposti tarkistaa avaimen olemassaoloa laukaisematta virhettä.

Viimeaikaisemmissa ohjelmointiyhteyksissä kielet kuten Python tarjoavat sisäänrakennetun tuen assosiatiivisille taulukoille (joita Pythonissakin kutsutaan sanakirjoiksi) ilman tarvetta lisätä ulkoisia viitteitä. Tämä sisäänrakennettu tuki yksinkertaistaa prosessia ja tarjoaa enemmän kehittyneitä ominaisuuksia suoraan. Kuitenkin VBA:n puitteissa ja erityisesti Microsoft Officen sarjan automatisointiin suunnattuihin sovelluksiin, `Dictionary`-objektin käyttö pysyy tehokkaana ja relevanttina menetelmänä assosiatiivista taulukkomaisille datarakenteille.
