---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:08:07.871410-07:00
description: "Kuinka: Visual Basic for Applications (VBA) -ohjelmoinnissa kompleksilukujen\
  \ k\xE4sittely voi olla hieman v\xE4hemm\xE4n suoraviivaista verrattuna kieliin,\
  \ joissa\u2026"
lastmod: '2024-03-13T22:44:56.393856-06:00'
model: gpt-4-0125-preview
summary: "Visual Basic for Applications (VBA) -ohjelmoinnissa kompleksilukujen k\xE4\
  sittely voi olla hieman v\xE4hemm\xE4n suoraviivaista verrattuna kieliin, joissa\
  \ on natiivi tuki niille."
title: "Ty\xF6skentely kompleksilukujen kanssa"
weight: 14
---

## Kuinka:
Visual Basic for Applications (VBA) -ohjelmoinnissa kompleksilukujen käsittely voi olla hieman vähemmän suoraviivaista verrattuna kieliin, joissa on natiivi tuki niille. Voit kuitenkin hallita kompleksisia toimintoja luomalla funktioita tai käyttämällä olemassa olevia kirjastofunktioita. Tutkitaan perusesimerkkiä kompleksilukujen yhteenlaskusta, vähennyksestä, kertolaskusta ja jakolaskusta:

```vb
' Funktio kompleksilukujen yhteenlaskemiseen
Function AddComplex(x As String, y As String) As String
    Dim real1 As Double, imag1 As Double
    Dim real2 As Double, imag2 As Double
    
    ' Eriytetään reaali- ja imaginääriosat kompleksiluvuista
    real1 = Val(Split(x, "+")(0))
    imag1 = Val(Split(x, "+")(1))
    real2 = Val(Split(y, "+")(0))
    imag2 = Val(Split(y, "+")(1))
    
    ' Suoritetaan yhteenlasku
    AddComplex = (real1 + real2) & "+" & (imag1 + imag2) & "i"
End Function

' Esimerkki käytöstä
Sub ExampleUsage()
    Dim tulos As String
    tulos = AddComplex("3+2i", "1+7i")
    Debug.Print "Yhteenlaskun tulos: " & tulos  ' Tulostus: Yhteenlaskun tulos: 4+9i
End Sub
```

Vaikka tämä osoittaa yhteenlaskun, vastaavia lähestymistapoja voidaan mukauttaa vähennyslaskuun, kertolaskuun ja jakolaskuun. Kompleksisten toimintojen osalta perusaritmetiikan ylittävissä tapauksissa kannattaa ehkä tutkia ulkoisia kirjastoja tai integroida muita ratkaisuja, jotka tukevat kompleksilukutoimintoja luontaisemmin.

## Syväsukellus:
VBA ei sisällä sisäänrakennettua tukea kompleksiluvuille, mikä on osa-alue, jossa se jää jälkeen kielistä, kuten Python, jossa on kompleksilukuluokka (`complex`), tai C++, jolla on Standard Template Library (`std::complex`). Historiallisesti VBA:ssa tarve käsitellä suoraan kompleksilukuja on verrattain harvinaista, koska sitä käytetään usein automatisointiin, Office-sovellusten manipulointiin ja tehtäviin, jotka perinteisesti eivät vaadi monimutkaisia matemaattisia laskutoimituksia. Kun VBA suunniteltiin ja kehitettiin, sen käyttötapaukset kohdistuivat pääasiassa liiketoiminnan sovelluksiin eikä tieteelliseen laskentaan, mikä saattaa selittää tämän puutteen.

Tehtäviin, jotka vaativat laajaa kompleksilukujen manipulointia, ohjelmoijat saattavat hyötyä enemmän matemaattisesti suuntautuneen kielen käytöstä. Kuitenkin niille, jotka ovat sitoutuneet tai rajoitetut VBA:n käyttöön, oman funktion kirjoittaminen (kuten esiteltiin) tai integroituminen ohjelmistoihin, joilla on nämä kyvyt (kuten MATLAB tai jossain määrin Excel itse), ovat toteuttamiskelpoisia polkuja eteenpäin. Huolimatta sen rajoituksista, luovat ratkaisut ja ulkoiset integraatiot voivat laajentaa VBA:n käyttöaluetta alueille, joille sitä ei alun perin suunniteltu, mukaan lukien kompleksilukujen käsittely.
