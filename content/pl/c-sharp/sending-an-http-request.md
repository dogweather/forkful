---
title:                "C#: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach komunikacja między aplikacjami i systemami jest nieodzowna, dlatego istotnym elementem programowania jest możliwość wysyłania zapytań HTTP. Pozwala to na wymianę danych oraz integrację różnych aplikacji. W tym artykule, dalej przedstawimy jak w prosty sposób skorzystać z mechanizmu wysyłania HTTP request w języku C#.

## Jak to zrobić

Podstawowym sposobem na wysłanie zapytania HTTP jest użycie klasy `HttpClient`, która jest dostępna w przestrzeni nazw `System.Net.Http`. Obejmuje ona wiele metod, które mogą być wykorzystane do różnych rodzajów zapytań, takich jak GET, POST, PUT, DELETE. Poniżej znajduje się przykładowa implementacja wysłania GET request do strony https://google.com.

```C#
using System;
using System.Net.Http;

class Program
{
	static async Main()
	{
		HttpClient client = new HttpClient();
		var response = await client.GetAsync("https://google.com");
		if(response.IsSuccessStatusCode)
		{
			var result = await response.Content.ReadAsStringAsync();
			Console.WriteLine(result); //wypisanie odpowiedzi
		}
	}
}
```

W powyższym przykładzie utworzona została instancja klasy `HttpClient`, a następnie wywołana metoda `GetAsync` z podanym adresem url. Jeśli zapytanie zakończy się sukcesem, pobierana jest zawartość odpowiedzi i wyświetlana na ekranie.

## Deep Dive

Dodatkowo, `HttpClient` oferuje możliwość spersonalizowania zapytań poprzez dodanie nagłówków, lub znacznie bardziej rozbudowanymi parametrami. Możliwe jest również używanie wyższego poziomu abstrakcji, takiego jak klasa `HttpWebRequest`, która udostępnia jeszcze więcej kontroli nad zapytaniami.

Podczas wysyłania zapytania, bardzo ważne jest również obsłużenie ewentualnych błędów. W przypadku niepowodzenia wysłania zapytania, zostaną rzucone odpowiednie wyjątki, które należy obsłużyć w odpowiedni sposób.

## Zobacz również

- [Microsoft Docs - HttpClient Class](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient?view=net-5.0)
- [Microsoft Docs - HttpWebRequest Class](https://docs.microsoft.com/en-us/dotnet/api/system.net.httpwebrequest?view=net-5.0)
- [W3Schools - HTTP Requests in C#](https://www.w3schools.com/cs/cs_http.asp)