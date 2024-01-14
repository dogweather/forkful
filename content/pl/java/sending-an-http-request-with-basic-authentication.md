---
title:                "Java: Wysyłanie żądania HTTP z uwierzytelnieniem podstawowym"
simple_title:         "Wysyłanie żądania HTTP z uwierzytelnieniem podstawowym"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach wiele aplikacji wymaga od nas uwierzytelnienia w celu dostępu do pewnych funkcji lub danych. Aby zapewnić bezpieczeństwo i poufność, często stosuje się metodę uwierzytelnienia za pomocą podstawowej autoryzacji HTTP. W tym artykule dowiesz się, jak wysyłać zapytania HTTP z podstawową autoryzacją w języku Java.

## Jak

Kodowanie zapytań HTTP z podstawową autoryzacją jest bardzo proste w języku Java. Wystarczy utworzyć obiekt klasy `HttpURLConnection` i ustawić odpowiednie parametry, takie jak adres URL żądania oraz metoda zapytania. Następnie, w celu dodania podstawowej autoryzacji, należy ustawić nagłówek `Authorization` z wartością `"Basic"`, a następnie kodować login i hasło w formacie base64 i dodać je do nagłówka.

Poniżej przedstawiamy przykładowy kod wraz z wyjściem:

```Java
URL url = new URL("http://example.com/api/users");
HttpURLConnection connection = (HttpURLConnection) url.openConnection();
connection.setRequestMethod("GET");
String userCredentials = "username:password";
String basicAuth = "Basic " + new String(Base64.getEncoder().encode(userCredentials.getBytes()));
connection.setRequestProperty("Authorization", basicAuth);
int responseCode = connection.getResponseCode();
System.out.println("Response code: " + responseCode);
```

Wyjście dla powyższego kodu powinno mieć postać:

```text
Response code: 200
```

## Głębsze zagłębianie się

Podstawowa autoryzacja HTTP jest bardzo prosta do wprowadzenia, ale nie jest również najlepszym sposobem na uwierzytelnianie. Hasło nadal jest przesyłane w postaci tekstu, co czyni je podatnym na przechwycenie przez niepowołane osoby. Dlatego też zaleca się stosowanie innych metod uwierzytelniania, takich jak uwierzytelnienie oparte na tokenach.

## Zobacz również

- [Dokumentacja API HTTP w języku Java](https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html)
- [Podstawowa autoryzacja HTTP na stronie MDN](https://developer.mozilla.org/pl/docs/Web/HTTP/Authentication#Basic_authentication_scheme)