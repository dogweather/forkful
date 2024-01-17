---
title:                "Eine HTTP-Anfrage senden"
html_title:           "C#: Eine HTTP-Anfrage senden"
simple_title:         "Eine HTTP-Anfrage senden"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Was ist eine HTTP-Anfrage und warum ist es wichtig?

Eine HTTP-Anfrage ist eine Methode, um Daten von einem Server zu erhalten. Programmierer verwenden dies, um mit APIs und anderen Web-Services zu interagieren, um Daten für ihre Anwendungen zu erhalten.

# Wie geht's:

Um mit C# eine HTTP-Anfrage zu senden, benötigst du die Klasse "HttpClient" aus der Standardbibliothek. Innerhalb dieser Klasse gibt es eine Methode mit dem Namen "SendAsync", die wir verwenden, um eine Anfrage zu senden. Hier ist ein Beispiel für eine GET-Anfrage:

```C#
var client = new HttpClient();
var response = await client.GetAsync("https://www.example.com/");
```

Um eine POST-Anfrage zu senden, verwenden wir die entsprechende Methode "PostAsync" und übergeben die Daten als Parameter.

# Tiefer gehen:

HTTP-Anfragen sind Teil des Hypertext Transfer Protocols, das ursprünglich für den Austausch von Daten in Web-Anwendungen entwickelt wurde. Alternativ können Programmierer auch das ältere und weniger sichere Protokoll FTP verwenden, um Dateien zu übertragen. Bei der Implementierung von HTTP-Anfragen ist es wichtig, auf die Antwortstatuscodes und die korrekte Formatierung der Daten zu achten.

# Siehe auch:

- Offizielle Dokumentation zu HttpClient in C#: https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient?view=netcore-3.1
- Ein interaktives Tutorial zum Senden von HTTP-Anfragen mit C#: https://www.tutorialsteacher.com/csharp/csharp-http-get-post
- Ein Vergleich von HTTP und FTP-Protokollen: https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/Evolution_of_HTTP#File_Transfer_Protocols