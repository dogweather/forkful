---
date: 2024-01-20 17:59:47.152601-07:00
description: "How to: W C# u\u017Cywamy `HttpClient`. Sp\xF3jrz."
lastmod: '2024-03-13T22:44:35.405242-06:00'
model: gpt-4-1106-preview
summary: "W C# u\u017Cywamy `HttpClient`."
title: "Wysy\u0142anie \u017C\u0105dania HTTP"
weight: 44
---

## How to:
W C# używamy `HttpClient`. Spójrz:

```csharp
using System;
using System.Net.Http;
using System.Threading.Tasks;

class Program
{
    static async Task Main()
    {
        // Tworzenie klienta
        using HttpClient client = new HttpClient();

        try
        {
            // Wysyłanie żądania GET
            HttpResponseMessage response = await client.GetAsync("http://example.com");
            response.EnsureSuccessStatusCode(); // Rzuca wyjątek, jeśli nie 2XX
            string responseBody = await response.Content.ReadAsStringAsync();

            Console.WriteLine(responseBody);
        }
        catch (HttpRequestException e)
        {
            Console.WriteLine("\nWyjątek:");
            Console.WriteLine(e.Message);
        }
    }
}
```

Odpowiedź serwera pojawi się w konsoli – jako tekst strony `example.com`.

## Deep Dive


### Historia
Klasa `HttpClient` pojawiła się w .NET Framework 4.5. Zastąpiła starsze narzędzia jak `WebRequest` – z większym naciskiem na asynchroniczność i wydajność.

### Alternatywy
Poza `HttpClient`, można też używać `WebClient` albo niskopoziomowych socketów. Jednak `WebClient` jest uznawany za przestarzały, a sockety wymagają więcej pracy i wiedzy.

### Szczegóły
`HttpClient` jest zoptymalizowany do ponownego użytku. Stwórz raz i używaj wielokrotnie. Pamiętaj o `async-await` dla płynności i unikania zablokowania wątku. Kontrola błędów jest kluczowa – `EnsureSuccessStatusCode` i obsługa `HttpRequestException`.

## See Also
- [Oficjalna dokumentacja `HttpClient`](https://docs.microsoft.com/pl-pl/dotnet/api/system.net.http.httpclient)
- [Artykuł o zarządzaniu połączeniami w `HttpClient`](https://aspnetmonsters.com/2016/08/2016-08-27-httpclientwrong/)
