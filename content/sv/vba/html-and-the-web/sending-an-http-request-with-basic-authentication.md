---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:02:14.063287-07:00
description: "Att skicka en HTTP-beg\xE4ran med grundl\xE4ggande autentisering i Visual\
  \ Basic for Applications (VBA) handlar om att komma \xE5t webbresurser som skyddas\
  \ av\u2026"
lastmod: '2024-03-13T22:44:37.742710-06:00'
model: gpt-4-0125-preview
summary: "Att skicka en HTTP-beg\xE4ran med grundl\xE4ggande autentisering i Visual\
  \ Basic for Applications (VBA) handlar om att komma \xE5t webbresurser som skyddas\
  \ av\u2026"
title: "Skicka en HTTP-beg\xE4ran med grundl\xE4ggande autentisering"
weight: 45
---

## Vad och varför?

Att skicka en HTTP-begäran med grundläggande autentisering i Visual Basic for Applications (VBA) handlar om att komma åt webbresurser som skyddas av användarnamn och lösenordsuppgifter. Programmerare gör detta för att interagera med säkra API:er eller webbtjänster inom sina VBA-drivna applikationer, såsom att automatisera uppgifter i Excel eller Access med data från säkerhetsinriktade slutpunkter.

## Hur man gör:

I VBA kan du använda biblioteket `Microsoft XML, v6.0` (MSXML2) för att skicka HTTP-begäranden med grundläggande autentisering. Detta innebär att du ställer in begäranstokens `"Authorization"`-header för att inkludera referenserna i ett base64-kodat format. Här är en steg-för-steg-guide:

1. **Referera till MSXML2**: Först, se till att ditt VBA-projekt refererar till biblioteket `Microsoft XML, v6.0`. I VBA-editorn, gå till Verktyg > Referenser och kryssa i `Microsoft XML, v6.0`.

2. **Skapa och skicka HTTP-begäran**: Använd följande VBA-kodsnutt som en guide. Ersätt `"your_username"` och `"your_password"` med dina faktiska referenser och justera URL:en vid behov.

    ```vb
    Dim XMLHttp As Object
    Set XMLHttp = CreateObject("MSXML2.XMLHTTP")
    Dim url As String
    url = "http://example.com/api/resource" ' Ersätt med den faktiska URL:en
    Dim base64Credentials As String
    base64Credentials = EncodeBase64("your_username:your_password")
    
    XMLHttp.Open "GET", url, False
    XMLHttp.setRequestHeader "Authorization", "Basic " & base64Credentials
    XMLHttp.send
    
    Debug.Print XMLHttp.responseText ' Skriver ut svaret till omedelbart fönster
    ```

3. **Koda referenser i base64**: VBA har ingen inbyggd funktion för base64-kodning, men du kan använda denna anpassade `EncodeBase64`-funktion:

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
    
Detta kommer att skicka en GET-begäran till `http://example.com/api/resource` med de angivna grundläggande autentiseringsreferenserna och skriva ut svaret.

## Fördjupning

Metoden som används här, även om den är effektiv för enkla användningsfall, bygger på schemat för grundläggande autentisering, som skickar referenser i ett lätt avkodningsbart format (base64-kodning är inte kryptering). På grund av sin sårbarhet, särskilt i icke-HTTPS-sammanhang, rekommenderas inte grundläggande autentisering för att överföra känslig information över internet utan ytterligare säkerhetslager som SSL/TLS.

Historiskt sett var grundläggande autentisering en av de första metoderna som utvecklades för att kontrollera åtkomst till webbresurser. Idag är säkrare och mer flexibla autentiseringsstandarder, såsom OAuth 2.0, generellt att föredra för nya applikationer. Med tanke på VBA:s begränsningar och externa beroenden som krävs för mer avancerade autentiseringsmetoder använder utvecklare ofta VBA i interna eller mindre säkerhetskritiska miljöer eller använder det som ett steg för att snabbt prototypa idéer.

När du använder VBA för HTTP-begäranden, kom ihåg att varje version av MSXML-biblioteket kan stödja olika funktioner och säkerhetsstandarder. Använd alltid den mest aktuella versionen som är kompatibel med din applikation för att säkerställa bättre säkerhet och prestanda. Dessutom, överväg de miljömässiga begränsningarna och potentiellt föråldrade funktioner när du väljer VBA för nya projekt, särskilt de som kräver säker HTTP-kommunikation. Andra programmeringsmiljöer eller språk kan erbjuda mer robusta, säkra och underhållbara lösningar för liknande uppgifter.
