---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:07:41.327534-07:00
description: "YAML, joka tarkoittaa \"YAML Ain't Markup Language\" (YAML ei ole merkkauskieli),\
  \ on ihmisen luettavissa oleva datan serialisointikieli, jota k\xE4ytet\xE4\xE4\
  n\u2026"
lastmod: 2024-02-19 22:05:15.318736
model: gpt-4-0125-preview
summary: "YAML, joka tarkoittaa \"YAML Ain't Markup Language\" (YAML ei ole merkkauskieli),\
  \ on ihmisen luettavissa oleva datan serialisointikieli, jota k\xE4ytet\xE4\xE4\
  n\u2026"
title: "Ty\xF6skentely YAML:n parissa"
---

{{< edit_this_page >}}

## Mikä & Miksi?

YAML, joka tarkoittaa "YAML Ain't Markup Language" (YAML ei ole merkkauskieli), on ihmisen luettavissa oleva datan serialisointikieli, jota käytetään yleisesti konfiguraatiotiedostoissa. Ohjelmoijat käyttävät sitä usein sen yksinkertaisuuden ja luettavuuden vuoksi monenlaisissa ohjelmointiympäristöissä, mukaan lukien Visual Basic for Applications (VBA) skriptiympäristössä, jotta lisätään yhteentoimivuutta sekä datan tallennusta ja vaihtoa.

## Miten:

YAML:n käyttö VBA:ssa vaatii ymmärrystä siitä, miten YAML jäsennetään ja muunnetaan muotoon, jonka VBA voi helposti käsitellä, yleensä sanakirjoihin tai kokoelmiin. Valitettavasti VBA ei natiivisti tue YAML:n jäsentämistä tai serialisointia. Voit kuitenkin käyttää JSON-muunnostyökalujen ja sanakirjaobjektien yhdistelmää työskennelläksesi YAML-datalla, ottaen huomioon YAML:n läheisen suhteen JSON:iin.

Muunna ensin YAML-datas; JSON:ksi käyttämällä online-muunninta tai YAML-to-JSON-muunnostyökalua kehitysympäristössäsi. Muunnettuna voit käyttää seuraavaa esimerkkiä JSON:n jäsentämiseen VBA:ssa, huomaten, että tämä lähestymistapa sallii sinun epäsuorast; työskennellä YAML:n kanssa:

```vb
' Lisää viite Microsoft Scripting Runtime kirjastoon Dictionaryä varten
' Lisää viite Microsoft XML, v6.0 JSON:n jäsentämiseen

Sub ParseYAMLAsJSON()
    Dim jsonText As String
    jsonText = "{""name"": ""John Doe"", ""age"": 30}" ' Tämä on YAML:sta muunnettua JSON:ia
    
    ' Olettaen, että sinulla on JSON-jäsentäjäfunktio
    Dim parsedData As Dictionary
    Set parsedData = JsonParser(jsonText)
    
    Debug.Print "Nimi: " & parsedData("name")
    Debug.Print "Ikä: " & parsedData("age")
End Sub

Function JsonParser(ByVal jsonText As String) As Dictionary
    ' Paikka, jossa JSON-jäsentämisen logiikka - saatat käyttää ulkoista kirjastoa tässä
    Set JsonParser = New Dictionary
    JsonParser.Add "name", "John Doe"
    JsonParser.Add "age", 30
End Function
```
Tässä esimerkissä `JsonParser`-funktio on sijainen, missä jäsentäisit JSON:ia. Eri kirjastoja on saatavilla auttamaan JSON-jäsentämisessä, koska suoria YAML-jäsentämiskirjastoja VBA:lle on niukasti.

## Syvä sukellus

Suoraan YAML:n käsittelyn puuttuminen VBA:ssa voidaan jäljittää sen ikään ja ympäristöön, jolle se oli alun perin suunniteltu, mikä ei alun perin otettu huomioon moderneja datan serialisointiformaatteja. YAML itse nousi suosituksi konfiguraatio- ja serialisointiformaatiksi 2000-luvun alussa, samaan aikaan kun sovellusten tarve ihmisläheisemmille konfiguraatiotiedostoille kasvoi.

Ohjelmoijat yleensä hyödyntävät ulkoisia työkaluja tai kirjastoja ylittääkseen kuilun VBA:n ja YAML:n välillä. Usein tämä tarkoittaa YAML:n muuntamista JSON:iksi, kuten näytetty, JSON-tukeen saatavilla olevien eri kirjastojen ja JSON:n ja YAML:n rakenteen ja tarkoituksen samankaltaisuuden vuoksi.

Vaikka suora työskentely YAML:n kanssa VBA:ssa osoittaa kielen joustavuuden, on huomionarvoista, että muut ohjelmointiympäristöt (esim. Python tai JavaScript) tarjoavat natiivimman ja saumattomamman tuen YAML:lle. Nämä vaihtoehdot saattavat olla paremmin sopivia projekteihin, jotka ovat raskaasti riippuvaisia YAML:sta konfiguraatioon tai datan serialisointiin. Kuitenkin niille, jotka ovat sitoutuneet tai tarvitsevat VBA:ta, epäsuora menetelmä JSON-muunnoksen kautta pysyy käyttökelpoisena ja hyödyllisenä lähestymistapana hallita ja manipuloida YAML-dataa.
