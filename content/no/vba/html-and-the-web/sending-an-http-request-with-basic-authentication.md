---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:03:14.144203-07:00
description: "\xC5 sende en HTTP-foresp\xF8rsel med grunnleggende autentisering i\
  \ Visual Basic for Applications (VBA) dreier seg om \xE5 f\xE5 tilgang til webressurser\
  \ som er\u2026"
lastmod: '2024-03-13T22:44:40.619224-06:00'
model: gpt-4-0125-preview
summary: "\xC5 sende en HTTP-foresp\xF8rsel med grunnleggende autentisering i Visual\
  \ Basic for Applications (VBA) dreier seg om \xE5 f\xE5 tilgang til webressurser\
  \ som er beskyttet av brukernavn- og passordopplysninger."
title: "\xC5 sende en HTTP-foresp\xF8rsel med grunnleggende autentisering"
weight: 45
---

## Hva & Hvorfor?

Å sende en HTTP-forespørsel med grunnleggende autentisering i Visual Basic for Applications (VBA) dreier seg om å få tilgang til webressurser som er beskyttet av brukernavn- og passordopplysninger. Programmerere gjør dette for å samhandle med sikre API-er eller webtjenester innenfor deres VBA-drevne applikasjoner, som å automatisere oppgaver i Excel eller Access med data fra sikrede endepunkter.

## Hvordan:

I VBA kan du bruke biblioteket `Microsoft XML, v6.0` (MSXML2) for å sende HTTP-forespørsler med grunnleggende autentisering. Dette innebærer å sette `"Authorization"`-headeren i forespørselen for å inkludere legitimasjonen i et base64-kodet format. Her er en steg-for-steg veiledning:

1. **Referer til MSXML2**: Først, sørg for at VBA-prosjektet ditt refererer til `Microsoft XML, v6.0`-biblioteket. I VBA-redigereren, gå til Verktøy > Referanser og sjekk `Microsoft XML, v6.0`.

2. **Opprett og send HTTP-forespørselen**: Bruk følgende VBA-kodesnutt som en veiledning. Bytt ut `"your_username"` og `"your_password"` med dine faktiske legitimasjonsopplysninger og juster URL-en etter behov.

    ```vb
    Dim XMLHttp As Object
    Set XMLHttp = CreateObject("MSXML2.XMLHTTP")
    Dim url As String
    url = "http://example.com/api/resource" ' Erstatt med den faktiske URL-en
    Dim base64Credentials As String
    base64Credentials = EncodeBase64("your_username:your_password")
    
    XMLHttp.Open "GET", url, False
    XMLHttp.setRequestHeader "Authorization", "Basic " & base64Credentials
    XMLHttp.send
    
    Debug.Print XMLHttp.responseText ' Skriver ut responsen til umiddelbart vindu
    ```

3. **Kode legitimasjonen i base64**: VBA har ikke en innebygd funksjon for base64-koding, men du kan bruke denne tilpassede `EncodeBase64`-funksjonen:

    ```vb
    Function EncodeBase64(tekst As String) As String
        Dim arrData() As Byte
        arrData = StrConv(tekst, vbFromUnicode)
        
        Dim objXML As MSXML2.DOMDocument60
        Dim objNode As MSXML2.IXMLDOMElement
        
        Set objXML = New MSXML2.DOMDocument60
        Set objNode = objXML.createElement("b64")
        
        objNode.dataType = "bin.base64"
        objNode.nodeTypedValue = arrData
        EncodeBase64 = objNode.Text
    End Function
    ```
   
Dette vil sende en GET-forespørsel til `http://example.com/api/resource` med de spesifiserte grunnleggende autentiseringslegitimasjonene og skrive ut responsen.

## Dypdykk

Tilnærmingen som brukes her, mens den er effektiv for enkle bruksmåter, er avhengig av grunnleggende autentiseringsskjema, som sender legitimasjon i et lett dekodet format (base64-koding er ikke kryptering). På grunn av sårbarheten, spesielt i ikke-HTTPS-kontekster, anbefales ikke grunnleggende autentisering for å overføre sensitiv data over internett uten ytterligere sikkerhetslag som SSL/TLS.

Historisk sett var grunnleggende autentisering en av de første metodene som ble utviklet for å kontrollere tilgang til webressurser. I dag foretrekkes sikrere og mer fleksible autentiseringsstandarder, som OAuth 2.0, generelt for nye applikasjoner. Gitt VBAs begrensninger og de eksterne avhengighetene som kreves for mer avanserte autentiseringsmetoder, bruker utviklere ofte VBA i interne eller mindre sikkerhetskritiske miljøer eller bruker det som et trinn for å raskt prototypere ideer.

Når du bruker VBA for HTTP-forespørsler, husk at hver versjon av MSXML-biblioteket kan støtte forskjellige funksjoner og sikkerhetsstandarder. Bruk alltid den nyeste versjonen som er kompatibel med applikasjonen din for å sikre bedre sikkerhet og ytelse. I tillegg, vurder de miljømessige begrensningene og potensielle avskrevne funksjoner når du velger VBA for nye prosjekter, spesielt de som krever sikker HTTP-kommunikasjon. Andre programmeringsmiljøer eller språk kan tilby mer robuste, sikre og vedlikeholdbare løsninger for lignende oppgaver.
