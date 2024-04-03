---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:08:51.491594-07:00
description: "Ohjelmoinnissa testien kirjoittaminen tarkoittaa tiettyjen menettelyjen\
  \ luomista koodin toimivuuden ja suorituskyvyn varmistamiseksi, varmistamaan, ett\xE4\
  \u2026"
lastmod: '2024-03-13T22:44:56.404723-06:00'
model: gpt-4-0125-preview
summary: "Ohjelmoinnissa testien kirjoittaminen tarkoittaa tiettyjen menettelyjen\
  \ luomista koodin toimivuuden ja suorituskyvyn varmistamiseksi, varmistamaan, ett\xE4\
  \ ne toimivat odotetusti erilaisissa olosuhteissa."
title: Testien kirjoittaminen
weight: 36
---

## Miten:
Vaikka Visual Basic for Applications (VBA) ei sisälläkään valmista testausrunkoa, joka olisi verrattavissa kielissä kuten Python tai JavaScript saatavilla oleviin, voit silti toteuttaa yksinkertaisia testimenettelyjä koodisi eheyden tarkistamiseksi. Tässä on esimerkki havainnollistamaan:

Oletetaan, että sinulla on VBA:ssa funktio, joka lisää kaksi lukua:

```basic
Function AddNumbers(x As Integer, y As Integer) As Integer
    AddNumbers = x + y
End Function
```

Tämän funktion testaamiseksi voit kirjoittaa toisen menettelyn, joka vahvistaa sen tuloksen odotettujen tulosten vastaavuuden:

```basic
Sub TestAddNumbers()
    Dim result As Integer
    result = AddNumbers(5, 10)
    If result = 15 Then
        MsgBox "Testi läpäisty!", vbInformation
    Else
        MsgBox "Testi epäonnistui. Odotettiin 15, mutta saatiin " & result, vbCritical
    End If
End Sub
```

`TestAddNumbers` suorittaminen näyttää viestiruudun, jossa ilmoitetaan, läpäisikö testi vai epäonnistuiko se, perustuen funktion tulokseen. Vaikka tämä on yksinkertaistettu skenaario, voit rakentaa monimutkaisempia testejä lisäämällä silmukoita, erilaisia syöttöarvoja ja testaamalla useampia funktioita.

## Syväsukellus
Tässä näytetty lähestymistapa testien kirjoittamiseen VBA:ssa on manuaalinen ja vailla monien muiden ohjelmointiympäristöjen saatavilla olevien kehittyneempien testausrunkojen ominaisuuksia, kuten automaattiset testiajot, asennus/purkumenettelyt ja testitulosten integroitu raportointi. Ennen yksikkötestauskehysten ja testivetoinen kehittämisen (TDD) laajempaa omaksumista, manuaaliset testausmenettelyt, samankaltaiset kuin kuvattu, olivat yleisiä. Vaikka tämä menetelmä on yksinkertainen ja voi olla tehokas pienissä projekteissa tai oppimistarkoituksessa, se ei ole skaalautuva tai tehokas suuremmissa projekteissa tai tiimeissä.

Rikkaampia kehitystyökaluja tukevissa ympäristöissä ohjelmoijat kääntyvät usein .NET-sovelluksille tarkoitettujen NUnit-kehysten tai Java-sovelluksille tarkoitettujen JUnit-kehysten puoleen, jotka tarjoavat kattavat työkalut systemaattiseen testien kirjoittamiseen ja suorittamiseen. Nämä kehykset tarjoavat edistyneitä ominaisuuksia, kuten testitulosten vahvistamisen, mock-objektien asettamisen ja koodikattavuuden mittaamisen.

VBA-kehittäjät, jotka etsivät kehittyneempiä testausmahdollisuuksia, lähin vaihtoehto voisi olla ulkoisten työkalujen hyödyntäminen tai integroituminen muihin ohjelmointiympäristöihin. Jotkut kehittäjät käyttävät VBA:ta yhdistettynä Exceliin tallentaakseen testiskenaarioita ja -tuloksia manuaalisesti. Vaikka tämä ei ole yhtä kätevää tai automatisoitua kuin omistetun testausrungon käyttäminen, nämä menetelmät voivat osittain kuromaan umpeen aukon, auttaen ylläpitämään VBA-ratkaisujen luotettavuutta monimutkaisissa tai kriittisissä sovelluksissa.
