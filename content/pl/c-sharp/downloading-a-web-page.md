---
title:                "Pobieranie strony internetowej"
html_title:           "C#: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

"Ściąganie" strony internetowej oznacza pobieranie danych z serwera na swój lokalny komputer. Programiści robią to, aby analizować te dane, przetwarzać je i wykorzystywać w różnych celach.

## Jak to zrobić:

Operację pobierania strony internetowej w C# można zrealizować za pomocą klasy `HttpClient` zawartej w bibliotece `System.Net.Http`.

```C#
using System;
using System.Net.Http;
using System.Threading.Tasks;

class Program
{
    public static async Task Main()
    {
        var httpClient = new HttpClient();
        string responseBody = await httpClient.GetStringAsync("https://example.com");
        
        Console.WriteLine(responseBody);
    }
}
```

Oczekuje się, że wynikiem powyższego kodu będzie wyświetlenie źródłowego kodu HTML strony internetowej "https://example.com".

## Głębsze Zagadnienia:

1. **Historyczny Kontekst:** Klasa `HttpClient` zastąpiła starsze metody sieciowe, takie jak `WebClient` i `HttpWebRequest`.

2. **Alternatywy:** Istnieje wiele alternatyw dla `HttpClient`, w tym biblioteki zewnętrzne takie jak `RestSharp` i `Flurl`.

3. **Szczegóły Implementacji:** Gdy używasz `HttpClient` do pobierania strony, dane te są pobierane do pamięci, zanim zostaną przekazane do naszego kodu. To znaczy, że pobieranie bardzo dużych stron internetowych może być żmudne dla pamięci systemu.

## Zobacz Również:

1. [Dokumentacja HttpClient (.NET)](https://docs.microsoft.com/pl-pl/dotnet/api/system.net.http.httpclient?view=net-5.0)

2. [RestSharp](http://restsharp.org/)
   
3. [Flurl](https://flurl.dev/)