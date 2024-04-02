---
date: 2024-01-20 17:43:49.832622-07:00
description: "Das Herunterladen einer Webseite bedeutet das Abrufen des Inhalts \xFC\
  ber das Internet auf deinen Computer. Programmierer tun dies, um Daten zu sammeln,\u2026"
lastmod: '2024-03-13T22:44:53.887169-06:00'
model: gpt-4-1106-preview
summary: "Das Herunterladen einer Webseite bedeutet das Abrufen des Inhalts \xFCber\
  \ das Internet auf deinen Computer. Programmierer tun dies, um Daten zu sammeln,\u2026"
title: Webseite herunterladen
weight: 42
---

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
