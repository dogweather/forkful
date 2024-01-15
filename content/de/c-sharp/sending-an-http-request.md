---
title:                "Versenden einer http-Anfrage"
html_title:           "C#: Versenden einer http-Anfrage"
simple_title:         "Versenden einer http-Anfrage"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Warum

Das Versenden von HTTP-Anfragen ist ein wichtiger Teil der modernen Webentwicklung und ermöglicht es uns, Daten von externen Servern abzurufen. Ob Sie nun eine API integrieren, Daten für Ihre Anwendung sammeln oder einfach nur die Funktionen einer Website nutzen wollen, das Senden von HTTP-Anfragen ist unerlässlich.

## Wie geht das?

Um eine HTTP-Anfrage in C# zu senden, benötigen Sie die `HttpClient`-Klasse aus dem `System.Net.Http`-Namespace. Hier ist ein Beispiel, wie Sie eine GET-Anfrage an eine externe API senden können und die Antwort als JSON-Objekt verarbeiten können:

```C#
using System;
using System.Net.Http;
using System.Threading.Tasks;
using Newtonsoft.Json;

string url = "https://example.com/api/someData";

// HttpClient initialisieren
HttpClient client = new HttpClient();

// GET-Anfrage senden und Antwort erhalten
HttpResponseMessage response = await client.GetAsync(url);

// Antwort als string auslesen
string data = await response.Content.ReadAsStringAsync();

// JSON-Objekt aus der Antwort erstellen
dynamic result = JsonConvert.DeserializeObject(data);

// Daten aus dem JSON-Objekt abrufen
string name = result.name;
int age = result.age;

// Output anzeigen
Console.WriteLine("Name: " + name);
Console.WriteLine("Alter: " + age);
```

Output:

```
Name: Max Mustermann
Alter: 30
```

Es ist wichtig zu beachten, dass `HttpClient` asynchron arbeitet, daher verwenden wir das `await`-Keyword und die `async`-Methode, um die Antwort abzuwarten und die Daten zu verarbeiten.

## Tieferer Einblick

Im obigen Beispiel haben wir eine GET-Anfrage gesendet, aber `HttpClient` unterstützt auch andere HTTP-Methoden wie POST, PUT, DELETE, etc. Sie können auch Header-Informationen und Anfrage-Body-Daten hinzufügen, je nach den Anforderungen der API.

Es ist auch wichtig zu verstehen, dass das Senden von HTTP-Anfragen keine einwegige Kommunikation ist. Nachdem die Anfrage gesendet wurde, erhält man eine Antwort von dem Server, auf den man zugegriffen hat. Diese Antwort enthält normalerweise einen Statuscode, der anzeigt, ob die Anfrage erfolgreich war oder nicht, sowie die Daten, die der Server zurückgibt. Diese Daten können in verschiedenen Formaten sein, wie z.B. XML oder JSON, und müssen entsprechend verarbeitet werden.

Eine weitere wichtige Sache ist die Fehlerbehandlung. Wenn die Anfrage aus irgendeinem Grund fehlschlägt, müssen Sie in der Lage sein, damit umzugehen und eine entsprechende Fehlermeldung auszugeben oder alternative Aktionen durchzuführen.

## Siehe auch

- [Microsoft Docs - Using HttpClient for HTTP requests](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient?view=netcore-3.1)
- [Tutorialspoint - C# HttpClient Class](https://www.tutorialspoint.com/csharp/csharp_httpclient.htm)
- [Codeburst - How to Make HTTP Requests in C#](https://codeburst.io/how-to-make-http-requests-in-c-291d93608d77)