---
title:                "Webseite herunterladen"
aliases:
- de/c-sharp/downloading-a-web-page.md
date:                  2024-01-20T17:43:49.832622-07:00
model:                 gpt-4-1106-preview
simple_title:         "Webseite herunterladen"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Herunterladen einer Webseite bedeutet das Abrufen des Inhalts über das Internet auf deinen Computer. Programmierer tun dies, um Daten zu sammeln, Inhalte zu prüfen oder Webanwendungen zu testen.

## Wie geht das?
Mit C# kannst du eine Webseite mit einigen Zeilen Code herunterladen. Hier ist ein einfaches Beispiel mit `HttpClient`, einer Klasse, die in der .NET-Standardbibliothek enthalten ist.

```C#
using System;
using System.Net.Http;
using System.Threading.Tasks;

class Program
{
    static async Task Main()
    {
        using (HttpClient client = new HttpClient())
        {
            try
            {
                string url = "https://example.com";
                HttpResponseMessage response = await client.GetAsync(url);
                response.EnsureSuccessStatusCode();
                string responseBody = await response.Content.ReadAsStringAsync();
                Console.WriteLine(responseBody);
            }
            catch(HttpRequestException e)
            {
                Console.WriteLine("\nException Caught!");
                Console.WriteLine("Message :{0} ", e.Message);
            }
        }
    }
}
```

Wenn du dieses Beispiel ausführst, solltest du den HTML-Inhalt von `https://example.com` in deiner Konsole sehen.

## Deep Dive
Früher nutzte man `WebClient` oder `HttpWebRequest`, um Webseiten herunterzuladen. `HttpClient` ist moderner, bietet asynchronen Support und ist wiederverwendbar, was weniger Ressourcen verbraucht.

Alternativen zum direkten Code-Ansatz sind Tools wie `cURL` oder Spezialbibliotheken wie `HtmlAgilityPack`, wenn du mehr mit dem HTML-Inhalt machen möchtest, z.B. Parsing oder Scraping.

Wichtig beim Herunterladen von Webseiten ist das richtige Handhaben von Netzwerkfehlern und Ausnahmen und das Respektieren von `robots.txt`, einer Datei, die angibt, welche Bereiche einer Webseite durch Bots nicht besucht werden sollten.

## Siehe auch
- [HttpClient-Klassendokumentation](https://docs.microsoft.com/de-de/dotnet/api/system.net.http.httpclient?view=net-6.0)
- [Übersicht über asynchrone Programmierung](https://docs.microsoft.com/de-de/dotnet/csharp/async)
- [HtmlAgilityPack auf GitHub](https://github.com/zzzprojects/html-agility-pack)
- [cURL Website](https://curl.haxx.se/)
