---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:02:41.981893-07:00
description: "HTTP-pyynn\xF6n l\xE4hett\xE4minen perusautentikoinnilla Visual Basic\
  \ for Applications (VBA) -ohjelmassa liittyy verkkoresurssien k\xE4ytt\xE4miseen,\
  \ jotka on suojattu\u2026"
lastmod: '2024-03-13T22:44:56.400216-06:00'
model: gpt-4-0125-preview
summary: "HTTP-pyynn\xF6n l\xE4hett\xE4minen perusautentikoinnilla Visual Basic for\
  \ Applications (VBA) -ohjelmassa liittyy verkkoresurssien k\xE4ytt\xE4miseen, jotka\
  \ on suojattu\u2026"
title: "HTTP-pyynn\xF6n l\xE4hett\xE4minen perusautentikoinnilla"
---

{{< edit_this_page >}}

## Mikä ja Miksi?

HTTP-pyynnön lähettäminen perusautentikoinnilla Visual Basic for Applications (VBA) -ohjelmassa liittyy verkkoresurssien käyttämiseen, jotka on suojattu käyttäjätunnuksen ja salasanan avulla. Ohjelmoijat tekevät tämän vuorovaikuttaakseen turvallisten APIen tai web-palveluiden kanssa VBA:lla varustetuissa sovelluksissaan, kuten automatisoidakseen tehtäviä Excelissä tai Accessissa tietojen avulla turvatuista päätepisteistä.

## Kuinka:

VBA:ssa voit käyttää `Microsoft XML, v6.0` (MSXML2) kirjastoa lähettääksesi HTTP-pyynnöt perusautentikoinnilla. Tämä sisältää `"Authorization"`-otsikon asettamisen pyynnössä sisällyttämään tunnistetiedot base64-koodatussa muodossa. Tässä on askel askeleelta -opas:

1. **Viittaa MSXML2**: Ensin varmista, että VBA-projektissasi on viittaus `Microsoft XML, v6.0` kirjastoon. VBA-editorissa, siirry kohtaan Työkalut > Viittaukset ja valitse `Microsoft XML, v6.0`.

2. **Luo ja lähetä HTTP-pyyntö**: Käytä seuraavaa VBA-koodiesimerkkiä ohjeena. Korvaa `"your_username"` ja `"your_password"` todellisilla tunnistetiedoillasi ja muuta URL tarvittaessa.

    ```vb
    Dim XMLHttp As Object
    Set XMLHttp = CreateObject("MSXML2.XMLHTTP")
    Dim url As String
    url = "http://example.com/api/resource" ' Korvaa oikealla URLilla
    Dim base64Credentials As String
    base64Credentials = EncodeBase64("your_username:your_password")
    
    XMLHttp.Open "GET", url, False
    XMLHttp.setRequestHeader "Authorization", "Basic " & base64Credentials
    XMLHttp.send
    
    Debug.Print XMLHttp.responseText ' Tulostaa vastauksen Välitön-ikkunaan
    ```

3. **Koodaa tunnistetiedot base64-muotoon**: VBA:ssa ei ole sisäänrakennettua toimintoa base64-koodaukseen, mutta voit käyttää tätä mukautettua `EncodeBase64`-funktiota:

    ```vb
    Function EncodeBase64(text As String) As String
        Dim arrData() As Byte
        arrData = StrConv(text, vbFromUnicode)
        
        Dim objXML As MSXML2.DOMDocument60
        Dim objNode As MSXML2.IXMLDOMElement
        
        Set objXML = New MSXML2.DOMDocument60
        Set objNode = objXML.createElement("b64")
        
        objNode.dataType = "bin.base64"
        objNode.nodeTypedValue = arrData
        EncodeBase64 = objNode.Text
    End Function
    ```
    
Tämä lähettää GET-pyynnön osoitteeseen `http://example.com/api/resource` määritetyillä perusautentikointitunnuksilla ja tulostaa vastauksen.

## Syväsukellus

Tässä käytetty lähestymistapa, vaikka se onkin tehokas yksinkertaisissa tapauksissa, nojaa perusautentikointiskeemaan, joka lähettää tunnistetiedot helposti dekoodattavassa muodossa (base64-koodaus ei ole salaus). Sen haavoittuvuuden vuoksi, erityisesti ei-HTTPS-yhteyksissä, perusautentikointia ei suositella arkaluonteisten tietojen siirtämiseen internetin yli ilman lisäturvakerroksia kuten SSL/TLS.

Historiallisesti perusautentikointi oli yksi ensimmäisistä kehitetyistä menetelmistä verkkoresurssien käyttöoikeuksien hallintaan. Nykyään uusille sovelluksille yleensä suositaan turvallisempia ja joustavampia autentikointistandardeja, kuten OAuth 2.0. VBA:n rajoitteiden ja edistyneempiin autentikointimenetelmiin vaadittavien ulkoisten riippuvuuksien vuoksi kehittäjät käyttävät usein VBA:ta sisäisissä tai vähemmän turvallisuuskriittisissä ympäristöissä tai käyttävät sitä nopeiden prototyyppien kehittämiseen.

Kun käytät VBA:ta HTTP-pyyntöihin, muista, että MSXML-kirjaston eri versiot saattavat tukea erilaisia ominaisuuksia ja turvallisuusstandardeja. Käytä aina sovellukseesi sopivaa uusinta versiota varmistaaksesi paremman turvallisuuden ja suorituskyvyn. Lisäksi harkitse ympäristön rajoituksia ja mahdollisesti vanhentuneita ominaisuuksia valitessasi VBA:ta uusiin projekteihin, erityisesti niissä, jotka vaativat turvallista HTTP-viestintää. Muut ohjelmointiympäristöt tai -kielet voivat tarjota vastaaviin tehtäviin vankempia, turvallisempia ja ylläpidettävämpiä ratkaisuja.
