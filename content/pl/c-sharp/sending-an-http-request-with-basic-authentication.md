---
title:                "C#: Wysyłanie żądania http z podstawową autoryzacją"
simple_title:         "Wysyłanie żądania http z podstawową autoryzacją"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach komunikacja przez internet jest nieodłączną częścią naszego codziennego życia. Wiele aplikacji musi wymieniać informacje z serwerami, aby działać poprawnie. Istnieją różne sposoby przesyłania danych, jednak jeden z najczęściej używanych to protokół HTTP. Aby utrzymać bezpieczeństwo i kontrolę nad przesyłanymi danymi, często konieczne jest uwierzytelnianie. W tym artykule wyjaśnimy, dlaczego i jak używać podstawowej uwierzytelni w żądaniach HTTP w języku C#.

## Jak to zrobić

Do wysłania żądania HTTP z podstawową uwierzytelnią w C#, potrzebujemy tylko kilku linii kodu. Najpierw musimy utworzyć obiekt klasy `HttpClient` i przypisać mu adres URL, do którego chcemy wysłać żądanie. Następnie, w nagłówkach żądania, musimy dodać autoryzację podając nazwę użytkownika i hasło w formacie `username:password`. W przypadku poprawnie wykonanego żądania, otrzymamy jako odpowiedź kod stanu HTTP 200 (OK) oraz treść w odpowiedzi. Poniżej znajduje się przykładowy kod:

```C#
using System;
using System.Net.Http;

class Program
{
  static readonly HttpClient client = new HttpClient();
  
  static async Task Main()
  {
    try 
    {
      client.BaseAddress = new Uri("https://www.example.com/"); // Ustawienie adresu URL
      client.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue("Basic", Convert.ToBase64String(Encoding.UTF8.GetBytes("username:password"))); // Dodanie autoryzacji

      HttpResponseMessage response = await client.GetAsync("/api/someData"); // Wysłanie żądania GET na endpoint /api/someData
      
      response.EnsureSuccessStatusCode(); // Rzucenie wyjątku w przypadku niepowodzenia żądania
      string responseBody = await response.Content.ReadAsStringAsync(); // Pobranie treści odpowiedzi

      Console.WriteLine(responseBody); // Wyświetlenie treści odpowiedzi
    }
    catch(HttpRequestException e)
    {
      Console.WriteLine("Wystąpił błąd: " + e.Message);
    }
  }
}
```

Powyższy kod jest tylko przykładem i należy pamiętać o odpowiednim zabezpieczeniu hasła, gdy korzystamy z uwierzytelniania.

## Głębszy zanurzenie

Podstawowa uwierzytalność w żądaniach HTTP jest tylko jednym z wielu sposobów na zabezpieczanie naszych komunikacji. Istnieją również inne sposoby, takie jak uwierzytelnianie oparte o tokeny lub używanie protokołu HTTPS. Ważne jest, aby zawsze korzystać z odpowiednich zabezpieczeń do charakteru transmisji danych.

## Zobacz także

- [Dokumentacja Microsoft o klasie HttpClient](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
- [Więcej o standardzie HTTP i uwierzytelnianiu](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)