---
title:                "Lokiointi"
date:                  2024-02-01T21:56:15.127049-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lokiointi"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/vba/logging.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä ja miksi?

Logging Visual Basic for Applications (VBA) -ympäristössä tarkoittaa ohjelman suorituskäyttäytymisen tallentamista tiedostoon, konsoliin tai tietokantaan. Ohjelmoijat käyttävät lokia sovellustensa seurantaan, ongelmien diagnosointiin ja suorituskyvyn ymmärtämiseen.

## Kuinka:

VBAssa ei ole sisäänrakennettua lokituskehystä, kuten joissakin muissa kielissä. Kuitenkin yksinkertaisen lokitusmekanismin toteuttaminen on suoraviivaista. Alla on esimerkki siitä, kuinka luoda perustason tiedostoloki.

1. **Kirjoittaminen lokitiedostoon**: Tämä esimerkkifunktio, `LogMessage`, kirjoittaa viestejä tekstitiedostoon aikaleiman kera.

```basic
Sub LogMessage(message As String)
    Dim logFilePath As String
    Dim fileNum As Integer
    
    ' Määritä lokitiedoston polku
    logFilePath = ThisWorkbook.Path & "\log.txt"
    
    ' Saa seuraava vapaana oleva tiedostonumero
    fileNum = FreeFile()
    
    ' Avaa tiedosto lisätäksesi siihen
    Open logFilePath For Append As #fileNum
    
    ' Kirjoita aikaleima ja lokiviesti
    Print #fileNum, Now & ": " & message
    
    ' Sulje tiedosto
    Close #fileNum
End Sub
```

Lokitusta varten kutsu vain `LogMessage("Viestisi tähän")`. Tämä tuottaa *log.txt*-tiedostoon merkintöjä kuten:

```
30.4.2023 15:45:32: Viestisi tähän
```

2. **Lukeminen lokitiedostosta**: Lukeaksesi ja näyttääksesi lokitiedoston sisällön:

```basic
Sub ReadLogFile()
    Dim logFilePath As String
    Dim fileContent As String
    Dim fileNum As Integer
    
    logFilePath = ThisWorkbook.Path & "\log.txt"
    fileNum = FreeFile()
    
    ' Avaa tiedosto lukuun
    Open logFilePath For Input As #fileNum
    
    ' Lue koko tiedoston sisältö
    fileContent = Input(LOF(fileNum), fileNum)
    
    ' Sulje tiedosto
    Close #fileNum
    
    ' Näytä tiedoston sisältö
    MsgBox fileContent
End Sub
```

## Syväsukellus

Lokitus VBAssa, sen puuttuvan natiivin lokituskehyksen vuoksi, toteutetaan yleensä perus tiedosto-operaatioiden kautta tai hyödyntämällä ulkoisia COM-objekteja edistyneempiin tarpeisiin, kuten lokituksen tekemiseen tietokantaan tai vuorovaikutuksessa Windowsin tapahtumalokin kanssa. Historiallisesti lokitus VBAssa on ollut keino kiertää sen yksinkertaistettujen virheenkäsittely- ja debuggaustyökalujen asettamia rajoituksia. Vaikkakin tehokas, suoran tiedoston manipuloinnin käyttö lokitukseen on alkeellista ja voi olla tehotonta suurten datamäärien käsittelyssä tai suuren samanaikaisuuden alaisena. Tarkempaan lokituskykyyn ohjelmoijat kääntyvät usein ulkoisten kirjastojen puoleen tai integroituvat erityisesti lokitukseen suunniteltuihin järjestelmiin, kuten ELK-pino (Elasticsearch, Logstash, Kibana) tai Splunk, web-palvelukutsujen tai välitietokantojen kautta. Vaikka VBA ei tarjoakaan uudempien ohjelmointikielten moderneja mukavuuksia, sen kyvykkyyksien ja rajoitusten ymmärtäminen mahdollistaa ohjelmoijien tehokkaan hyödyntämisen lokitusta apuvälineenä sovellusten seurannassa ja diagnostiikassa.
