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

## Co i dlaczego? 
Pobieranie strony internetowej to proces pozyskiwania zawartości z adresu URL i przeniesienia jej do aplikacji. Programiści często korzystają z tego narzędzia, ponieważ pozwala ono na automatyczne przetwarzanie informacji i wykorzystanie ich w swoich programach.

## Jak to zrobić:
Poniżej znajdują się dwa przykłady kodu w języku C#, które prezentują jak można pobrać zawartość strony internetowej.

Przykład 1: 
```C#
using System;
using System.Net; // Biblioteka zawierająca klasy do pobierania danych z Internetu

class Program
{
    static void Main()
    {
        // Inicjalizacja klasy WebClient
        WebClient client = new WebClient();
        
        // Pobranie tekstowej zawartości strony
        string data = client.DownloadString("https://www.example.com");

        // Wyświetlenie pobranej zawartości
        Console.WriteLine(data);
    }
}
```
Wynik:
> <!DOCTYPE html> <html> <head> <title>Example Domain</title> <link rel="icon" href="https://www.example.com/favicon.ico" type="image/x-icon" /> <body> <h1>Example Domain</h1> <p>This domain is for use in illustrative examples in documents. You may use this domain in literature without prior coordination or asking for permission.</p> <p><a href="https://www.iana.org/domains/example">More information...</a></p> </body> </html>

Przykład 2:
```C#
using System;
using System.IO; // Biblioteka do obsługi plików

class Program
{
    static void Main()
    {
        // Pobieranie zawartości z adresu URL 
        string url = "https://www.example.com";
        
        // Inicjalizacja klasy HttpWebRequest
        HttpWebRequest request = (HttpWebRequest)WebRequest.Create(url);
        
        // Pobranie odpowiedzi i zapisanie jej w zmiennej response
        HttpWebResponse response = (HttpWebResponse)request.GetResponse();
        
        // Otworzenie strumienia danych i przypisanie do niego odpowiedzi
        StreamReader sr = new StreamReader(response.GetResponseStream());
        
        // Pobranie danych z otwartego strumienia 
        string data = sr.ReadToEnd();

        // Wyświetlenie pobranej zawartości 
        Console.WriteLine(data);

        // Zamknięcie strumienia
        sr.Close();
    }
}
```

Wynik:
> <!DOCTYPE html> <html> <head> <title>Example Domain</title> <link rel="icon" href="https://www.example.com/favicon.ico" type="image/x-icon" /> <body> <h1>Example Domain</h1> <p>This domain is for use in illustrative examples in documents. You may use this domain in literature without prior coordination or asking for permission.</p> <p><a href="https://www.iana.org/domains/example">More information...</a></p> </body> </html>

## Deep Dive:
Pobieranie zawartości strony internetowej jest powszechnie używane od początków Internetu. Jednym z popularnych sposobów na pobieranie jest wykorzystywanie biblioteki WebClient lub HttpWebRequest. Istnieją również inne narzędzia i biblioteki, które mogą być wykorzystywane w celu pobrania danych z Internetu.

W celu uniknięcia problemów z pobieraniem zawartości strony zaleca się stosowanie narzędzi, które zapewniają obsługę protokołu HTTPS. Najważniejsze jest również zachowanie zasad etykiety dotyczących praw autorskich i licencji, gdy pobieramy dane z Internetu.

## Zobacz również:
- [Dokumentacja C# - WebRequest](https://docs.microsoft.com/en-us/dotnet/api/system.net.webrequest?view=net-5.0)
- [Dokumentacja C# - WebClient](https://docs.microsoft.com/en-us/dotnet/api/system.net.webclient?view=net-5.0)
- [6 Ways to Download Files with C#](https://www.dreamincode.net/forums/topic/189761-6-ways-to-download-files-with-c%23/)