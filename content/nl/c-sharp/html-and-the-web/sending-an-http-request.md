---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:31.699876-07:00
description: 'Hoe: C# maakt het versturen van HTTP-verzoeken eenvoudig met `HttpClient`.
  Hier is het skelet van een GET-verzoek.'
lastmod: '2024-03-13T22:44:50.806858-06:00'
model: gpt-4-0125-preview
summary: C# maakt het versturen van HTTP-verzoeken eenvoudig met `HttpClient`.
title: Een HTTP-verzoek verzenden
weight: 44
---

## Hoe:
C# maakt het versturen van HTTP-verzoeken eenvoudig met `HttpClient`. Hier is het skelet van een GET-verzoek:

```C#
using System;
using System.Net.Http;
using System.Threading.Tasks;

class Program
{
    static async Task Main(string[] args)
    {
        using HttpClient client = new HttpClient();
        HttpResponseMessage response = await client.GetAsync("http://example.com");
        response.EnsureSuccessStatusCode();
        string responseBody = await response.Content.ReadAsStringAsync();
        
        Console.WriteLine(responseBody);
    }
}
```

Voorbeelduitvoer (afgekort):
```
<!doctype html>
<html>
<head>
    <title>Voorbeeld Domein</title>
...
```

## Diepere Duik
`HttpClient` werd geïntroduceerd in .NET Framework 4.5 om HTTP-communicatie gemakkelijker te maken. Daarvoor moest je waarschijnlijk worstelen met de klassen `HttpWebRequest` en `HttpWebResponse`, die omslachtiger waren.

Er zijn andere manieren om HTTP-verzoeken te versturen in C#. `RestSharp` en `Flurl` zijn twee populaire bibliotheken van derden die een vloeiendere interface en extra functies bieden. Maar `HttpClient` is meestal meer dan genoeg voor de meeste behoeften.

Wat implementatie betreft, is `HttpClient` ontworpen om hergebruikt te worden voor meerdere verzoeken. Het voor elk verzoek instantiëren ervan kan het aantal beschikbare sockets uitputten onder zware belasting. Let altijd, en ik bedoel altijd, op de juiste afhandeling van `HttpClient`-instanties om lekken van middelen te voorkomen.

## Zie Ook
- Microsoft's `HttpClient` documentatie: [https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
- Beste praktijken voor HttpClient: [https://aspnetmonsters.com/2016/08/2016-08-27-httpclientwrong/](https://aspnetmonsters.com/2016/08/2016-08-27-httpclientwrong/)
- Interactie met RESTful API met `RestSharp`: [http://restsharp.org/](http://restsharp.org/)
- Vloeiende HTTP (HTTP made fluent) met `Flurl`: [https://flurl.dev/](https://flurl.dev/)
