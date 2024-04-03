---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:55:29.651196-07:00
description: "Kuinka: VBA:ssa virheenk\xE4sittely toteutetaan tyypillisesti k\xE4\
  ytt\xE4m\xE4ll\xE4 `On Error` -lauseketta, joka ohjeistaa VBA:ta miten edet\xE4\
  \ kun virhe tapahtuu.\u2026"
lastmod: '2024-03-13T22:44:56.408776-06:00'
model: gpt-4-0125-preview
summary: "VBA:ssa virheenk\xE4sittely toteutetaan tyypillisesti k\xE4ytt\xE4m\xE4\
  ll\xE4 `On Error` -lauseketta, joka ohjeistaa VBA:ta miten edet\xE4 kun virhe tapahtuu."
title: "Virheiden k\xE4sittely"
weight: 16
---

## Kuinka:
VBA:ssa virheenkäsittely toteutetaan tyypillisesti käyttämällä `On Error` -lauseketta, joka ohjeistaa VBA:ta miten edetä kun virhe tapahtuu. Yleisimmät virheenkäsittelystrategiat sisältävät `On Error GoTo` -merkin, `On Error Resume Next` ja `On Error GoTo 0`.

**Esimerkki 1: Käyttäen `On Error GoTo`**

Tämä lähestymistapa mahdollistaa ohjelman ohjaamisen tiettyyn koodiosioon, joka on merkitty välittömästi virheen havaitsemisen jälkeen.

```vb
Sub ErrorHandlerExample()
    On Error GoTo ErrHandler
    Dim intDivision As Integer

    intDivision = 5 / 0 ' Tämä aiheuttaa nollalla jakamisen virheen

    Exit Sub
ErrHandler:
    MsgBox "An Error Occurred: " & Err.Description, vbCritical, "Error!"
    Resume Next
End Sub
```

Tässä esimerkissä mikä tahansa ajonaikainen virhe laukaisee siirtymisen `ErrHandler`-kohtaan, näyttää virheilmoituksen ja sen jälkeen jatkaa seuraavalla rivillä virheen jälkeen.

**Esimerkki 2: Käyttäen `On Error Resume Next`**

Tämä strategia ohjeistaa VBA:ta jatkamaan seuraavan koodirivin suorittamista, vaikka virhe tapahtuisi, mikä voi olla hyödyllistä odotettujen vaarattomien virheiden kohdalla tai kun suunnittelet käsitteleväsi virheen myöhemmin suorituksessa.

```vb
Sub ResumeNextExample()
    On Error Resume Next
    Dim intDivision As Integer
    intDivision = 5 / 0 ' Tämä ei pysäytä ohjelmaa; virhe ohitetaan
    
    ' Tarkista, tapahtuiko virhe
    If Err.Number <> 0 Then
        MsgBox "An Error Occurred: " & Err.Description, vbExclamation, "Handled Error"
        ' Nollaa virhe
        Err.Clear
    End If
End Sub
```

Tässä tapauksessa ohjelma ei keskeydy virheeseen; se tarkistaa, tapahtuiko virhe, käsittelee sen jos niin on, ja sitten nollaa virheen.

## Syväsukellus
Historiallisesti virheenkäsittely ohjelmointikielissä on kehittynyt yksinkertaisista goto-lauseista monimutkaisempiin mekanismeihin kuten poikkeuksiin (exceptions) kielissä kuten Java ja C#. VBA:n virheenkäsittely, vaikka ei olekaan yhtä voimakas tai joustava kuin modernit poikkeuskäsittelyt, palvelee tarkoitustaan kielen sovelluksessa Microsoft Office -ympäristöjen tehtävien automatisoinnissa.

VBA:n virheenkäsittelyn ensisijainen rajoitus piilee sen jokseenkin kömpelössä ja manuaalisessa lähestymistavassa, joka vaatii virheenkäsittelykoodin huolellista sijoittelua ja selkeää ymmärrystä suorituksen kulusta. Nykyaikaiset ohjelmointikielet tarjoavat tyypillisesti elegantimpia ratkaisuja, kuten try-catch-lohkot, jotka automaattisesti hoitavat virran siirtymisen virheenkäsittelykoodiin ilman manuaalisia tarkistuksia tai hyppyjä koodin suorituksessa.

Huolimatta näistä rajoituksista, VBA:n virheenkäsittelymekanismit soveltuvat useimpiin automatisointitehtäviin ja kun niitä käytetään oikein, ne voivat merkittävästi vähentää käsittelemättömien virheiden aiheuttamien ongelmien todennäköisyyttä käyttäjille. Lisäksi VBA:n virheenkäsittelyn ymmärtäminen voi tarjota näkemyksiä vanhempiin ohjelmointiparadigmoihin ja virheenkäsittelystrategioiden kehitykseen ohjelmistokehityksessä.
