---
title:                "Wysyłanie zapytania http z podstawową autoryzacją"
date:                  2024-01-20T18:01:09.381171-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wysyłanie zapytania http z podstawową autoryzacją"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Wysyłanie żądania HTTP z podstawowym uwierzytelnieniem to sposób na autoryzację użytkownika poprzez przesyłanie loginu i hasła w nagłówku HTTP. Programiści używają tego mechanizmu, by dostęp do zasobów sieciowych był ograniczony i bezpieczny.

## Jak to zrobić:
```C#
using System;
using System.Net;
using System.Net.Http;
using System.Text;
using System.Threading.Tasks;

class Program
{
    static async Task Main(string[] args)
    {
        var client = new HttpClient();

        // Tworzenie danych uwierzytelniania
        var credentials = Encoding.ASCII.GetBytes("username:password");
        client.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue("Basic", Convert.ToBase64String(credentials));

        try
        {
            // Wysłanie żądania GET z podstawowym uwierzytelnieniem
            HttpResponseMessage response = await client.GetAsync("http://example.com/protected-resource");
            response.EnsureSuccessStatusCode();
            string responseBody = await response.Content.ReadAsStringAsync();
            Console.WriteLine(responseBody);
        }
        catch (HttpRequestException e)
        {
            Console.WriteLine("\nWyjątek!");
            Console.WriteLine("Wiadomość :{0} ", e.Message);
        }
    }
}
```
### Przykładowy wynik:
```
<treść chronionego zasobu>
```

## Deep Dive
Podstawowe uwierzytelnienie HTTP zalicza się do najprostszych technik kontroli dostępu - jest wbudowane w protokół HTTP od wczesnych jego wersji. Niestety, przez przesyłanie danych w nieszyfrowanej postaci jest uważane za niewystarczająco bezpieczne, zwłaszcza w publicznym internecie. Alternatywy jak OAuth lub JWT (JSON Web Tokens) oferują większe bezpieczeństwo poprzez bardziej skomplikowane mechanizmy sprawdzania tożsamości.

Warto również pamiętać o tym, że dołączanie danych logowania do każdego żądania może wpłynąć na wydajność aplikacji. HTTP/2 i HTTP/3 przynoszą poprawy w zarządzaniu połączeniami, co może częściowo ten problem rozwiązać.

W C# dobrą praktyką jest wykorzystywanie klasy `HttpClient` do asynchronicznego wysyłania żądań. Użycie `AuthenticationHeaderValue` pozwala na łatwą manipulację nagłówkami żądania dla uwierzytelnienia.

## Zobacz także:
- [Dokumentacja `HttpClient` w MSDN](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
- [Podstawy podstawowego uwierzytelnienia HTTP](https://tools.ietf.org/html/rfc7617)
- [OAuth 2.0](https://oauth.net/2/)
- [JSON Web Tokens](https://jwt.io/introduction/)
