---
date: 2024-01-20 17:59:18.422486-07:00
description: "HTTP-Anfragen erm\xF6glichen die Kommunikation zwischen deiner Software\
  \ und Webdiensten. Programmierer senden diese Anfragen, um Daten zu holen oder zu\u2026"
lastmod: '2024-02-25T18:49:50.945475-07:00'
model: gpt-4-1106-preview
summary: "HTTP-Anfragen erm\xF6glichen die Kommunikation zwischen deiner Software\
  \ und Webdiensten. Programmierer senden diese Anfragen, um Daten zu holen oder zu\u2026"
title: Einen HTTP-Request senden
---

{{< edit_this_page >}}

## Was & Warum?
HTTP-Anfragen ermöglichen die Kommunikation zwischen deiner Software und Webdiensten. Programmierer senden diese Anfragen, um Daten zu holen oder zu senden, APIs zu nutzen und auf Webressourcen zuzugreifen.

## How to:
Hier ein Beispiel, wie man in C# eine einfache GET-Anfrage sendet und die Antwort ausliest:

```C#
using System;
using System.Net.Http;
using System.Threading.Tasks;

class Program
{
    static async Task Main(string[] args)
    {
        using (var client = new HttpClient())
        {
            HttpResponseMessage response = await client.GetAsync("http://example.com");
            
            if (response.IsSuccessStatusCode)
            {
                string responseBody = await response.Content.ReadAsStringAsync();
                Console.WriteLine(responseBody);
            }
            else
            {
                Console.WriteLine("Error: " + response.StatusCode);
            }
        }
    }
}
```

Das erwartete Ergebnis ist der HTML-Inhalt der angeforderten Webseite oder ein Fehlerstatuscode.

## Deep Dive
Bereits in den 90er Jahren, mit dem Aufkommen des World Wide Web, wurden HTTP-Anfragen wichtig. Damals wurden sie mittels einfacher Tools und Befehlszeilenprogramme gesendet. Heute ist das Versenden einer HTTP-Anfrage ohne die eingebauten Bibliotheken von Programmiersprachen, wie C#'s `HttpClient`, kaum noch vorstellbar.

Alternativen zum `HttpClient` in C# sind die Klassen `WebClient` und `HttpWebRequest`, die allerdings als veraltet gelten und nur noch aus Kompatibilitätsgründen vorhanden sind. Beim `HttpClient` handelt es sich um eine moderne, asynchrone API, die eine nachhaltigere und flexiblere Handhabung von Netzwerkanfragen bietet.

Im Zusammenhang mit `HttpClient` sollten auch `HttpRequestMessage` und `HttpResponseMessage` erwähnt werden, die detailliertere Kontrolle über Anfragen und Antworten ermöglichen. Außerdem sollten Entwickler mit Konzepten wie asynchroner Programmierung und dem Umgang mit HTTP-Statuscodes vertraut sein, um effektiv mit HTTP-Anfragen umgehen zu können.

## See Also

- [HttpClient Class Documentation](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient?view=net-6.0)
- [Asynchronous programming with async and await](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/async/)
- [HTTP Status Codes](https://developer.mozilla.org/de/docs/Web/HTTP/Status)
