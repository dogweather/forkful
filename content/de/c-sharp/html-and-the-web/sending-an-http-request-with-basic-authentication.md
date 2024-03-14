---
date: 2024-01-20 18:01:05.824648-07:00
description: "HTTP-Anfragen mit Basisauthentifizierung erlauben es einem Client, sich\
  \ mit Benutzername und Passwort gegen\xFCber einem Server zu authentifizieren.\u2026"
lastmod: '2024-03-13T22:44:53.888063-06:00'
model: gpt-4-1106-preview
summary: "HTTP-Anfragen mit Basisauthentifizierung erlauben es einem Client, sich\
  \ mit Benutzername und Passwort gegen\xFCber einem Server zu authentifizieren.\u2026"
title: HTTP-Anfragen mit Basisauthentifizierung senden
---

{{< edit_this_page >}}

## Was & Warum?
HTTP-Anfragen mit Basisauthentifizierung erlauben es einem Client, sich mit Benutzername und Passwort gegenüber einem Server zu authentifizieren. Programmierer nutzen dies, um sicheren Zugang zu Webressourcen zu gewährleisten.

## Anleitung:

```C#
using System;
using System.Net.Http;
using System.Text;
using System.Threading.Tasks;

public class BasicAuthExample
{
    public static async Task Main()
    {
        using (var client = new HttpClient())
        {
            // URL des Webdienstes
            var url = "https://example.com/api/data";
            
            // Benutzername und Passwort
            var username = "user";
            var password = "pass";
            
            // Konvertiere Benutzername und Passwort in Base64
            var credentials = Convert.ToBase64String(Encoding.ASCII.GetBytes($"{username}:{password}"));
            
            // Füge den Authorization-Header hinzu
            client.DefaultRequestHeaders.Authorization = new System.Net.Http.Headers.AuthenticationHeaderValue("Basic", credentials);
            
            // Sende die GET-Anfrage
            var response = await client.GetAsync(url);
            
            // Lese die Antwort und zeige sie an
            var content = await response.Content.ReadAsStringAsync();
            Console.WriteLine(content);
        }
    }
}
```

Muster-Ausgabe nach Ausführen könnte sein:
```
{
  "data": "Geheime Informationen"
}
```

## Deep Dive:
Die Basisauthentifizierung ist ein Authentifizierungsprotokoll in HTTP, bei dem Benutzername und Passwort im Header jeder Anfrage kodiert mitgeschickt werden. Seit den frühen Tagen des Internets wird dies genutzt. Es ist einfach, aber nicht das Sicherste, weil die Credentials leicht entschlüsselbar sind, falls keine Verschlüsselung wie HTTPS verwendet wird.

Alternativen zur Basisauthentifizierung sind OAuth, Token-basierte Authentifizierung oder API-Schlüssel. Diese bieten zusätzliche Sicherheitsmechanismen und Flexibilität.

Wichtig ist, dass bei jedem Request die Credentials mitgeschickt werden müssen, da HTTP ein zustandsloses Protokoll ist. Also macht man's leicht für die, die schnelle und simple Lösungen brauchen, aber es ist nicht ideal für Dienste, bei denen es auf erhöhte Sicherheit ankommt.

## Siehe Auch:

- Microsoft Docs zur HttpClient-Klasse in .NET: [HttpClient Class](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
- Eine Einführung in die verschiedenen Authentifizierungsstandards: [HTTP Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- Mehr zu HTTPS und seine Bedeutung für die Sicherheit: [HTTPS](https://en.wikipedia.org/wiki/HTTPS)
