---
title:                "Wysyłanie żądania http"
html_title:           "Arduino: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wysyłanie żądania HTTP to proces komunikacji, gdzie aplikacja klienta prosi o informacje od serwera. Programiści robią to, aby pobierać dane z serwerów, przesyłać dane do serwerów i integrować swoje aplikacje z serwisami internetowymi.

## Jak (How To):

W języku C# możliwe jest wysłanie żądania HTTP za pomocą klasy HttpClient. Poniżej przykład, o którym mówimy:

```C#
using System;
using System.Net.Http;
using System.Threading.Tasks;

class Program
{
    private static readonly HttpClient client = new HttpClient();

    static async Task Main()
    {
        HttpResponseMessage response = await client.GetAsync("http://api.example.com/data");
        response.EnsureSuccessStatusCode();
        string responseBody = await response.Content.ReadAsStringAsync();
        Console.WriteLine(responseBody);
    }
}
```

W wyjściu zobaczysz treść żądania HTTP zakończonej pomyślnie.

## Deep Dive

HttpClient to klasa wprowadzona w .NET Framework 4.5 w 2012 roku jako nowy model programowania dla wysyłania żądań HTTP. Jest bardziej elastyczny i wygodny w użyciu niż starsze klasy, takie jak WebClient.

Alternatywą dla HttpClient jest RestSharp, ktory oferuje bardziej rozbudowany zestaw funkcji, ale może nie być potrzebny w prostych przypadkach.

Jednocześnie warto pamiętać, że HttpClient jest projektowany do bycia wielokrotnie używaną długotrwałą instancją, a nie tworzonym na nowo dla każdego żądania. Jest to związane z obsługą połączeń przez HttpClient, które mogą pozostać otwarte po zakończeniu żądania.

## Zobacz także

1. Dokumentacja HttpClient dla .NET 5.0 [link](https://docs.microsoft.com/pl-pl/dotnet/api/system.net.http.httpclient)
2. Wprowadzenie do RestSharp [link](http://restsharp.org/)
3. Skąd wiesz, kiedy otworzyć i zamknąć połączenia HTTP [link](https://www.aspnetmonsters.com/2016/08/2016-08-27-httpclientwrong/)