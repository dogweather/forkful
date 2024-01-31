---
title:                "Wysyłanie żądania HTTP"
date:                  2024-01-20T17:59:47.152601-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wysyłanie żądania HTTP"

category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?
Wysyłanie żądania HTTP to komunikowanie się z serwerem – wysyłasz komendę, serwer odpowiada. Programiści robią to, by pobierać dane, wysyłać informacje, autentykować użytkowników – podstawa pracy sieciowej.

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
