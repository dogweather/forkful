---
title:                "Eine HTTP-Anfrage senden"
aliases:
- /de/vba/sending-an-http-request/
date:                  2024-02-01T22:01:55.489489-07:00
model:                 gpt-4-0125-preview
simple_title:         "Eine HTTP-Anfrage senden"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/vba/sending-an-http-request.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Das Senden einer HTTP-Anfrage in Visual Basic for Applications (VBA) beinhaltet den programmatischen Zugriff auf Webressourcen oder Webservices durch das Stellen von Anfragen über HTTP. Programmierer tun dies, um Daten abzurufen, mit Online-APIs zu interagieren oder Formulare programmatisch innerhalb ihrer VBA-fähigen Anwendungen wie Excel, Access oder benutzerdefinierten VBA-Lösungen zu übermitteln.

## Wie:

Der Schlüssel zum Senden einer HTTP-Anfrage in VBA ist die Nutzung der `Microsoft XML, v6.0` Bibliothek (oder älterer Versionen, abhängig von Ihrem System). Stellen Sie zunächst sicher, dass diese Referenz in Ihrem Projekt aktiviert ist, indem Sie in den VBA-Editor gehen und unter Extras > Verweise `Microsoft XML, v6.0` ankreuzen.

So senden Sie eine einfache HTTP-GET-Anfrage:

```vb
Dim httpRequest As Object
Set httpRequest = CreateObject("MSXML2.XMLHTTP.6.0")

With httpRequest
    .Open "GET", "https://api.example.com/data", False
    .Send
    If .Status = 200 Then
        Debug.Print .responseText
    Else
        Debug.Print "Fehler: " & .Status & " - " & .statusText
    End If
End With
```

Für eine POST-Anfrage, bei der wir Daten (z. B. JSON) an einen Server senden müssen:

```vb
Dim httpRequest As Object, postData As String
Set httpRequest = CreateObject("MSXML2.XMLHTTP.6.0")
postData = "{""key"":""value""}"

With httpRequest
    .Open "POST", "https://api.example.com/submit", False
    .setRequestHeader "Content-Type", "application/json"
    .Send postData
    If .Status = 200 Then
        Debug.Print .responseText
    Else
        Debug.Print "Fehler: " & .Status & " - " & .statusText
    End If
End With
```

Die Beispielausgabe für eine erfolgreiche Anfrage könnte ein JSON-String oder eine HTML-Seite sein, abhängig von der API oder Webseite, mit der Sie interagieren:

```
{"data": "Dies ist die Antwort vom Server"}
```

## Vertiefung

Die vorgestellte Methode nutzt das `MSXML2.XMLHTTP` Objekt, Teil der Microsoft XML Core Services (MSXML). Es wurde eingeführt, um VBA-Entwicklern eine Möglichkeit zur Durchführung von XML-basierten Operationen zu bieten und wurde mit der Zeit zu einem gängigen Werkzeug für HTTP-Anfragen, auch wenn nicht direkt mit XML-Daten gearbeitet wird. Trotz seines Alters bleibt es eine zuverlässige Option für einfache Webinteraktionen in VBA.

Allerdings fehlt VBA und seinen HTTP-Anfragemechanismen die Robustheit und Flexibilität moderner Programmierumgebungen. Beispielsweise ist die Handhabung asynchroner Anfragen oder das Arbeiten in Anwendungen, die fortgeschrittene HTTP-Funktionen benötigen (wie Websockets oder servergesendete Ereignisse), außerhalb des Umfangs von VBA. Bei komplexeren Web-Integrationsprojekten greifen Entwickler oft auf externe Bibliotheken oder Werkzeuge zurück oder automatisieren das Browserverhalten über Web-Scraping-Techniken, obwohl diese eher als Umwege statt als Lösungen gelten.

Sprachen und Umgebungen wie Python mit seiner `requests` Bibliothek oder JavaScript, das auf Node.js läuft, bieten leistungsfähigere und vielseitigere HTTP-Anfragefähigkeiten direkt aus dem Stand, einschließlich asynchroner Operationen, einfacherer JSON-Handhabung und umfangreicher Unterstützung für verschiedene Webtechnologien. Entwickler, die im Microsoft-Ökosystem verwurzelt sind, könnten den Umstieg auf PowerShell oder C# für Aufgaben in Betracht ziehen, die eine anspruchsvollere Webinteraktion erfordern, und dabei die umfangreichen Netzwerkprogrammierungsfunktionen von .NET nutzen.

Daher sind die HTTP-Anfragefähigkeiten von VBA zwar für einfache Abfragen und Datenerfassungsaufgaben ausreichend, die Erkundung von Alternativen wird jedoch entscheidend, wenn die Anforderungen Ihres Projekts sich auf die komplexe und moderne Weblandschaft zubewegen.
