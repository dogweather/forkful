---
title:                "Eine HTTP-Anforderung senden"
html_title:           "Bash: Eine HTTP-Anforderung senden"
simple_title:         "Eine HTTP-Anforderung senden"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

# HTTP-Anfragen senden in C#

## Was & Warum?

HTTP-Anfragen sind Verbindungsanfragen, die an einen Webserver gesendet werden, um Daten zu erhalten oder zu senden. Sie sind essentiell für die Kommunikation zwischen Client und Server und sind daher ein grundlegender Bestandteil der modernen Web-Entwicklung.

## So geht's:

Sie können in C# einfach HTTP-Anfragen mit der `HttpClient`-Klasse erstellen und senden. Hier ist ein einfaches Beispiel, wie man eine GET-Anfrage sendet:

```C#
using System;
using System.Net.Http;

class Program
{
    static async Task Main()
    {
        using (HttpClient client = new HttpClient())
        {
            HttpResponseMessage response = await client.GetAsync("https://api.example.com/data");
            response.EnsureSuccessStatusCode();
            string responseBody = await response.Content.ReadAsStringAsync();
            Console.WriteLine(responseBody);
        }
    }
}
```

Wenn das Programm läuft, sendet es eine GET-Anfrage an `api.example.com/data` und druckt die Antwort auf der Konsole aus.

## Tiefer Tauchen:

Historisch gesehen wurde die Kommunikation zwischen Client und Server über HTTP-Anfragen abgewickelt, und das ist immer noch der Fall. Aber heute gibt es viele Alternativen wie WebSockets, die eine bidirektionale Kommunikation ermöglichen.

Die `HttpClient`-Klasse in C# erzeugt eine HTTP-Anfrage durch Aufruf der Methode `GetAsync`, `PostAsync`, `PutAsync` oder `DeleteAsync`. Der Rückgabetyp dieser Funktionen ist ein `HttpResponseMessage`-Objekt, das Informationen über die HTTP-Antwort enthält.

Während `HttpClient` die am häufigsten verwendete Klasse für HTTP-Anfragen in C# ist, gibt es auch andere Optionen wie `WebRequest` und `WebClient`. Aber `HttpClient` bietet mehr Funktionen und Flexibilität, und ist daher die empfohlene Wahl.

## Siehe Auch:

Um mehr über HTTP-Anfragen und deren Verwendung in C# zu erfahren, finden Sie hier einige hilfreiche Links:

- [Microsoft-Dokumentation für HttpClient](https://docs.microsoft.com/de-de/dotnet/api/system.net.http.httpclient)
- [Grundlegende Informationen zu HTTP-Anfragen bei Mozilla](https://developer.mozilla.org/de/docs/Web/HTTP/Overview)
- [Vergleich der Klassen HttpClient, WebClient und WebRequest bei Stack Overflow](https://stackoverflow.com/questions/11828833/c-sharp-httpclient-vs-web-client)