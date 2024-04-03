---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:05:51.085222-07:00
description: "JSON (JavaScript Object Notation) on kevyt tiedonvaihtoformaatti, joka\
  \ on ihmisten luettavissa ja kirjoitettavissa helposti ja koneiden j\xE4sent\xE4\
  miseen ja\u2026"
lastmod: '2024-03-13T22:44:56.423807-06:00'
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation) on kevyt tiedonvaihtoformaatti, joka on\
  \ ihmisten luettavissa ja kirjoitettavissa helposti ja koneiden j\xE4sent\xE4miseen\
  \ ja tuottamiseen yksinkertainen."
title: "Ty\xF6skentely JSON:n kanssa"
weight: 38
---

## Mikä & Miksi?

JSON (JavaScript Object Notation) on kevyt tiedonvaihtoformaatti, joka on ihmisten luettavissa ja kirjoitettavissa helposti ja koneiden jäsentämiseen ja tuottamiseen yksinkertainen. Ohjelmoijat käyttävät JSONia datan välittämiseen palvelimen ja web-sovelluksen välillä tai tiedon tallentamiseen rakenteellisella, saavutettavalla tavalla erilaisissa ohjelmointiympäristöissä, mukaan lukien Visual Basic for Applications (VBA).

## Kuinka:

VBA ei natiivisti tue JSONin jäsentämistä tai tuottamista, joten käytämme skriptikieltä, kuten JScriptiä (ScriptControl-objektin kautta) JSON-merkkijonojen jäsentämiseen ja JSON-objektien rakentamiseen. Näin voit jäsentää JSON-merkkijonon VBA:ssa:

```basic
Function ParseJSON(ByVal jsonString As String) As Object
    Dim scriptControl As Object
    Set scriptControl = CreateObject("MSScriptControl.ScriptControl")
    scriptControl.Language = "JScript"
    
    scriptControl.Eval "var obj = (" & jsonString & ")"
    Set ParseJSON = scriptControl.CodeObject.obj
End Function

Sub DemoParseJSON()
    Dim jsonString As String
    jsonString = "{""name"":""John"", ""age"":30, ""city"":""New York""}"
    
    Dim parsed As Object
    Set parsed = ParseJSON(jsonString)
    
    MsgBox "Nimi: " & parsed.name & ", Ikä: " & parsed.age & ", Kaupunki: " & parsed.city
End Sub
```

JSONin tuottamiseen voisit käyttää samankaltaista lähestymistapaa, rakentaen JSON-merkkijonon yhdistelemällä:

```basic
Function GenerateJSON(name As String, age As Integer, city As String) As String
    GenerateJSON = "{""name"":""" & name & """, ""age"":" & age & ", ""city"":""" & city & """}"
End Function

Sub DemoGenerateJSON()
    Dim jsonString As String
    jsonString = GenerateJSON("Jane", 28, "Los Angeles")
    
    MsgBox jsonString
End Sub
```

## Syväsukellus

Esitetyt lähestymistavat hyödyntävät ScriptControlia JSONin käsittelyyn, käytännössä ulkoistaen työn JavaScript-moottorille. Tämä on luova kiertotapa, mutta ei välttämättä tehokkain tai modernein tapa työskennellä JSONin kanssa VBA-kontekstissa. Monimutkaisemmissa sovelluksissa tämä menetelmä saattaa muodostua hankalaksi ja tuoda mukanaan suorituskyvyn heikkenemistä tai turvallisuushuolia, koska ScriptControl suorittaa ympäristössä, jolla on täysi pääsy isäntätietokoneeseen.

Muut ohjelmointiympäristöt, kuten Python tai JavaScript, tarjoavat sisäänrakennetun tuen JSONille, tehdessään niistä sopivampia sovelluksille, jotka vaativat laajaa JSON-muokkausta. Nämä kielet tarjoavat kattavia kirjastoja, jotka helpottavat paitsi jäsentämistä ja tuottamista, myös JSON-datankyselyä ja -muotoilua.

Huolimatta näistä rajoituksista VBA:ssa, JSONin käsittelyn ymmärtäminen on tärkeää maailmassa, jossa web-pohjainen datanvaihto ja konfiguraatiotiedostot ovat pääosin JSON-muodossa. VBA-ohjelmoijille näiden tekniikoiden hallitseminen avaa mahdollisuuksia integroitumiseen web-APIihin, konfiguraatiotiedostojen tulkintaan tai jopa yksinkertaisten web-sovellusten rakentamiseen. Kuitenkin, kun projektit kasvavat monimutkaisuudessa tai vaativat suorituskykyä, kehittäjät saattavat harkita JSON-ystävällisempien ohjelmointiympäristöjen hyödyntämistä.
