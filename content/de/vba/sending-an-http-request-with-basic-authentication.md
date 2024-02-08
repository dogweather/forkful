---
title:                "Eine HTTP-Anfrage mit einfacher Authentifizierung senden"
aliases:
- de/vba/sending-an-http-request-with-basic-authentication.md
date:                  2024-02-01T22:02:29.748716-07:00
model:                 gpt-4-0125-preview
simple_title:         "Eine HTTP-Anfrage mit einfacher Authentifizierung senden"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/vba/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Das Senden einer HTTP-Anfrage mit grundlegender Authentifizierung in Visual Basic for Applications (VBA) dient dazu, auf Webressourcen zuzugreifen, die durch Benutzernamen und Passwort geschützt sind. Programmierer machen dies, um mit sicheren APIs oder Webdiensten innerhalb ihrer VBA-gestützten Anwendungen zu interagieren, wie zum Beispiel die Automatisierung von Aufgaben in Excel oder Access mit Daten von gesicherten Endpunkten.

## Wie geht das:

In VBA können Sie die `Microsoft XML, v6.0` (MSXML2) Bibliothek verwenden, um HTTP-Anfragen mit grundlegender Authentifizierung zu senden. Dies beinhaltet das Setzen des `"Authorization"` Headers der Anfrage, um die Anmeldeinformationen in einem base64-kodierten Format einzubeziehen. Hier ist eine schrittweise Anleitung:

1. **MSXML2 referenzieren**: Stellen Sie zunächst sicher, dass Ihr VBA-Projekt die `Microsoft XML, v6.0` Bibliothek referenziert. Gehen Sie im VBA-Editor zu Extras > Verweise und aktivieren Sie `Microsoft XML, v6.0`.

2. **Erstellen und senden der HTTP-Anfrage**: Verwenden Sie das folgende VBA-Code-Snippet als Leitfaden. Ersetzen Sie `"your_username"` und `"your_password"` durch Ihre tatsächlichen Anmeldeinformationen und passen Sie die URL wie benötigt an.

    ```vb
    Dim XMLHttp As Object
    Set XMLHttp = CreateObject("MSXML2.XMLHTTP")
    Dim url As String
    url = "http://example.com/api/resource" ' Ersetzen mit der tatsächlichen URL
    Dim base64Credentials As String
    base64Credentials = EncodeBase64("your_username:your_password")
    
    XMLHttp.Open "GET", url, False
    XMLHttp.setRequestHeader "Authorization", "Basic " & base64Credentials
    XMLHttp.send
    
    Debug.Print XMLHttp.responseText ' Gibt die Antwort im Direktfenster aus
    ```

3. **Anmeldeinformationen in base64 kodieren**: VBA verfügt nicht über eine integrierte Funktion für die base64-Kodierung, aber Sie können diese benutzerdefinierte `EncodeBase64` Funktion verwenden:

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
    
Dies sendet eine GET-Anfrage an `http://example.com/api/resource` mit den angegebenen grundlegenden Authentifizierungsinformationen und druckt die Antwort aus.

## Vertiefung

Der hier verwendete Ansatz, obwohl effektiv für einfache Anwendungsfälle, basiert auf dem Schema der Basic Authentication, das Anmeldeinformationen in einem leicht dekodierbaren Format (base64-Kodierung ist keine Verschlüsselung) sendet. Aufgrund seiner Anfälligkeit, insbesondere in Nicht-HTTPS-Kontexten, wird die Basic Authentication nicht empfohlen, um sensible Daten über das Internet zu übertragen, ohne zusätzliche Sicherheitsebenen wie SSL/TLS.

Historisch gesehen war die Basic Authentication eine der ersten Methoden, die für den Zugriffsschutz auf Webressourcen entwickelt wurde. Heute werden sicherere und flexiblere Authentifizierungsstandards, wie OAuth 2.0, allgemein für neue Anwendungen bevorzugt. Angesichts der Einschränkungen von VBA und der externen Abhängigkeiten, die für fortgeschrittenere Authentifizierungsmethoden erforderlich sind, verwenden Entwickler VBA oft in internen oder weniger sicherheitskritischen Umgebungen oder nutzen es als Sprungbrett, um Ideen schnell zu prototypisieren.

Wenn Sie VBA für HTTP-Anfragen verwenden, denken Sie daran, dass jede Version der MSXML-Bibliothek unterschiedliche Funktionen und Sicherheitsstandards unterstützen kann. Verwenden Sie immer die neueste mit Ihrer Anwendung kompatible Version, um eine bessere Sicherheit und Leistung zu gewährleisten. Berücksichtigen Sie außerdem die umgebungsbedingten Einschränkungen und potenziell veralteten Funktionen, wenn Sie VBA für neue Projekte wählen, insbesondere solche, die sichere HTTP-Kommunikation erfordern. Andere Programmierumgebungen oder Sprachen könnten robustere, sicherere und wartbarere Lösungen für ähnliche Aufgaben bieten.
