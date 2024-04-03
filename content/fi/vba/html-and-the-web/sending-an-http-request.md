---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:02:42.188267-07:00
description: "Kuinka: HTTP-pyynn\xF6n l\xE4hett\xE4misen avain VBA:ssa on `Microsoft\
  \ XML, v6.0` -kirjaston (tai vanhempien versioiden, riippuen j\xE4rjestelm\xE4st\xE4\
  si) k\xE4ytt\xF6.\u2026"
lastmod: '2024-03-13T22:44:56.396996-06:00'
model: gpt-4-0125-preview
summary: "HTTP-pyynn\xF6n l\xE4hett\xE4misen avain VBA:ssa on `Microsoft XML, v6.0`\
  \ -kirjaston (tai vanhempien versioiden, riippuen j\xE4rjestelm\xE4st\xE4si) k\xE4\
  ytt\xF6."
title: "HTTP-pyynn\xF6n l\xE4hett\xE4minen"
weight: 44
---

## Kuinka:
HTTP-pyynnön lähettämisen avain VBA:ssa on `Microsoft XML, v6.0` -kirjaston (tai vanhempien versioiden, riippuen järjestelmästäsi) käyttö. Varmista ensin, että tämä viittaus on otettu käyttöön projektissasi menemällä Työkalut > Viittaukset VBA-editorissa ja rastittamalla `Microsoft XML, v6.0`.

Tässä on kuinka lähetetään yksinkertainen HTTP GET -pyyntö:

```vb
Dim httpRequest As Object
Set httpRequest = CreateObject("MSXML2.XMLHTTP.6.0")

With httpRequest
    .Open "GET", "https://api.example.com/data", False
    .send
    If .Status = 200 Then
        Debug.Print .responseText
    Else
        Debug.Print "Virhe: " & .Status & " - " & .statusText
    End If
End With
```

POST-pyynnölle, jossa meidän täytyy lähettää dataa (esim. JSON) palvelimelle:

```vb
Dim httpRequest As Object, postData As String
Set httpRequest = CreateObject("MSXML2.XMLHTTP.6.0")
postData = "{""avain"":""arvo""}"

With httpRequest
    .Open "POST", "https://api.example.com/submit", False
    .setRequestHeader "Content-Type", "application/json"
    .send postData
    If .Status = 200 Then
        Debug.Print .responseText
    Else
        Debug.Print "Virhe: " & .Status & " - " & .statusText
    End If
End With
```

Näyte tuloste onnistuneelta pyynnöltä voi olla JSON merkkijono tai HTML-sivu, riippuen API:sta tai verkkosivusta, jonka kanssa olet vuorovaikutuksessa:

```
{"data": "Tämä on vastaus palvelimelta"}
```

## Syväsukellus
Esitelty menetelmä hyödyntää `MSXML2.XMLHTTP` -objektia, joka on osa Microsoft XML Core Services (MSXML) -palveluita. Se esiteltiin tarjoamaan VBA-kehittäjille tapa suorittaa XML-pohjaisia toimintoja ja, ajan myötä, siitä tuli yleinen työkalu HTTP-pyyntöihin, vaikka ei suoraan työskenneltäisi XML-datan parissa. Huolimatta sen iästä, se pysyy luotettavana vaihtoehtona yksinkertaisille web-vuorovaikutuksille VBA:ssa.

Kuitenkin VBA ja sen HTTP-pyyntömekanismit eivät ole yhtä vahvoja ja joustavia kuin modernit ohjelmointiympäristöt. Esimerkiksi asynkronisten pyyntöjen käsittely tai sovelluksissa työskentely, jotka vaativat edistyneitä HTTP-ominaisuuksia (kuten websockets tai palvelimelta lähetetyt tapahtumat), ovat VBA:n ulottumattomissa. Työskennellessään monimutkaisempien web-integrointiprojektien parissa, kehittäjät usein hyödyntävät ulkoisia kirjastoja tai työkaluja tai jopa automatisoivat selaimen käyttäytymistä web-kaavinnan tekniikoiden avulla, vaikka nämä ovatkin vain kiertotapoja eivätkä ratkaisuja.

Kielet ja ympäristöt, kuten Python sen `requests` kirjaston kanssa tai JavaScript, joka toimii Node.js:ssä, tarjoavat voimakkaampia ja monipuolisempia HTTP-pyyntöjen kyvykkyyksiä suoraan laatikosta, mukaan lukien asynkroniset toiminnot, helpomman JSON-käsittelyn ja laajan tuen erilaisille web-teknologioille. Kehittäjät, jotka ovat syvällä Microsoftin ekosysteemissä, saattavat harkita siirtymistä PowerShell- tai C#-tehtäviin, jotka vaativat monimutkaisempaa web-vuorovaikutusta, hyödyntäen .NETin laajaa verkkoprogrammointiominaisuuksia.

Näin ollen, vaikka VBA:n HTTP-pyyntöjen kyvykkyydet ovat riittäviä yksinkertaisiin tiedustelu- ja datanhakutehtäviin, vaihtoehtojen tutkiminen muuttuu välttämättömäksi, kun projektisi vaatimukset kehittyvät kohti monimutkaista ja modernia web-maisemaa.
