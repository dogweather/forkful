---
title:                "Wysyłanie żądania HTTP z podstawową autoryzacją"
html_title:           "C#: Wysyłanie żądania HTTP z podstawową autoryzacją"
simple_title:         "Wysyłanie żądania HTTP z podstawową autoryzacją"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Co i dlaczego? 
Wysyłanie żądań HTTP z podstawową autoryzacją to proces, który pozwala na uwierzytelnianie użytkownika w serwisie internetowym za pomocą loginu i hasła. Programiści często wykorzystują ten mechanizm w celu uzyskania dostępu do chronionych zasobów lub wykonywania operacji na serwerze.

## Jak to zrobić:
### Przykładowe kody i wyniki:
```C#
// Wysłanie żądania z autoryzacją:
var request = (HttpWebRequest)WebRequest.Create("https://example.com/protected");
request.Method = "GET";
request.Headers[HttpRequestHeader.Authorization] = "Basic " + Convert.ToBase64String(Encoding.ASCII.GetBytes("username:password"));
var response = (HttpWebResponse)request.GetResponse();
Console.WriteLine("Response status code: " + response.StatusCode);

// Otrzymanie odpowiedzi z danymi:
var responseStream = response.GetResponseStream();
using(var streamReader = new StreamReader(responseStream))
{
    var responseData = streamReader.ReadToEnd();
    Console.WriteLine("Response data: " + responseData);
}
```

## Głębszy wgląd:
### Kontekst historyczny:
Podstawowa autoryzacja została wprowadzona w protokole HTTP w 1999 roku i jest jednym z najstarszych i najprostszych sposobów uwierzytelniania w sieci. Jest często wykorzystywana w połączeniach typu "client-server" oraz w systemach API.

### Alternatywy:
Istnieją również inne metody uwierzytelniania w sieci, takie jak tokeny OAuth lub uwierzytelnianie oparte na kluczu API. Jednakże, podstawowa autoryzacja jest prosta w implementacji i jest często wykorzystywana w przypadku prostych zastosowań.

### Szczegóły implementacji:
Podstawowa autoryzacja jest realizowana poprzez dodanie nagłówka ```Authorization``` z odpowiednio zakodowaną nazwą użytkownika i hasłem w formacie ```username:password```, oddzielonym dwukropkiem oraz zakodowanym w formacie Base64. Po zoptymalizowaniu nagłówka wygeneruja on postać: ```Basic <base64(username:password)>```

## Zobacz także:
- https://tools.ietf.org/html/rfc2617 - specyfikacja podstawowej autoryzacji dla protokołu HTTP
- https://api.example.com/docs - przykładowa dokumentacja API wykorzystująca autoryzację