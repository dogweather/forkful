---
title:                "Wysyłanie żądania http"
html_title:           "C#: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

Co to jest i po co?

Wysyłanie żądania HTTP jest jedną z podstawowych umiejętności w programowaniu. Polega ono na wysyłaniu zapytania do serwera internetowego w celu pobrania danych lub wykonania określonej akcji. Programiści często korzystają z tego mechanizmu, aby uzyskać dostęp do zasobów online lub komunikować się z innymi aplikacjami.

Jak to zrobić:

```C#
// Example 1: Wysyłanie żądania GET do serwera Google
using System;
using System.Net.Http;

class Program
{
   static async Task Main()
   {
      // Utworzenie obiektu HttpClient
      using var client = new HttpClient();

      // Wywołanie metody GetAsync() z adresem URL, który chcemy odwiedzić
      using var response = await client.GetAsync("https://www.google.com");

      // Sprawdzenie statusu odpowiedzi
      Console.WriteLine($"Status code: {response.StatusCode}");

      // Pobranie zawartości odpowiedzi
      var content = await response.Content.ReadAsStringAsync();

      // Wyświetlenie pobranej zawartości
      Console.WriteLine(content);
   }
}

/* Wynik:
 Status code: 200
(...Zawartość strony Google...)
*/
```

```C#
// Example 2: Wysyłanie żądania POST z danymi formularza
using System;
using System.Net.Http;
using System.Collections.Generic;
using System.Collections.Specialized;

class Program
{
   static async Task Main()
   {
      // Utworzenie obiektu HttpClient
      using var client = new HttpClient();

      // Utworzenie kolekcji z danymi formularza
      var data = new NameValueCollection {
          { "username", "example" },
          { "password", "secret" }
      };

      // Wywołanie metody PostAsync() z adresem URL i danymi formularza
      using var response = await client.PostAsync("https://www.example.com/login", new FormUrlEncodedContent(data));

      // Sprawdzenie statusu odpowiedzi
      Console.WriteLine($"Status code: {response.StatusCode}");

      // Pobranie zawartości odpowiedzi
      var content = await response.Content.ReadAsStringAsync();

      // Wyświetlenie pobranej zawartości
      Console.WriteLine(content);
   }
}

/* Wynik:
 Status code: 200
(...Zawartość strony po zalogowaniu...)
*/
```

Pogłębiona analiza:

Wysyłanie żądania HTTP jest powszechnym sposobem pobierania danych lub komunikacji z serwerami internetowymi. Metoda ta została wprowadzona w protokole HTTP, który jest podstawowym mechanizmem komunikacji w sieci. Istnieje także wiele innych alternatywnych sposobów na korzystanie z serwisów online, takich jak protokół FTP czy RPC. Wysyłanie żądań HTTP jest jednak najczęściej stosowane w programowaniu, ponieważ jest prostsze w użyciu i obsługuje większość zasobów internetowych.

Zobacz też:

- Dokumentacja Microsoft dotycząca klasy HttpClient: https://docs.microsoft.com/pl-pl/dotnet/api/system.net.http.httpclient
- Poradnik na stronie MDN dotyczący żądań HTTP: https://developer.mozilla.org/pl/docs/Web/HTTP/Overview