---
title:                "Wysyłanie żądania http z autoryzacją podstawową."
html_title:           "Java: Wysyłanie żądania http z autoryzacją podstawową."
simple_title:         "Wysyłanie żądania http z autoryzacją podstawową."
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Dlaczego

Wysyłanie żądania HTTP z podstawową autoryzacją jest ważnym elementem w procesie komunikacji między klientem a serwerem. Pozwala ono na uwierzytelnienie użytkownika i zapewnienie bezpieczeństwa podczas przesyłania danych.

## Jak to zrobić

Aby wysłać żądanie HTTP z podstawową autoryzacją w Javie, możemy skorzystać z klasy `HttpURLConnection`. Najpierw musimy utworzyć obiekt klasy `URL` z adresem URL serwera, do którego chcemy się połączyć. Następnie tworzymy obiekt `HttpURLConnection` i ustalamy metodę zadania, w naszym przypadku będzie to `GET`.

```Java
URL url = new URL("https://example.com/api");
HttpURLConnection con = (HttpURLConnection) url.openConnection();
con.setRequestMethod("GET");
```

Kolejnym krokiem jest ustawienie podstawowej autoryzacji przy pomocy metody `setRequestProperty()`, gdzie podajemy nazwę nagłówka `Authorization` oraz zakodowane dane użytkownika w formacie Base64.

```Java
String user = "username";
String pass = "password";
String userPass = user + ":" + pass;
String auth = "Basic " + Base64.getEncoder().encodeToString(userPass.getBytes());
con.setRequestProperty("Authorization", auth);
```

W końcu możemy wysłać nasze żądanie i odczytać odpowiedź.

```Java
int status = con.getResponseCode();
BufferedReader in = new BufferedReader(new InputStreamReader(con.getInputStream()));
String inputLine;
StringBuffer content = new StringBuffer();

while ((inputLine = in.readLine()) != null) {
    content.append(inputLine);
}
in.close();

System.out.println("Status code: " + status);
System.out.println("Response body: " + content.toString());
```

Sample output:
```
Status code: 200
Response body: {"message": "Hello World!"}
```

## Deep Dive

Podstawowa autoryzacja w protokole HTTP jest jednym z najprostszych sposobów na uwierzytelnienie klienta. Polega ona na przesyłaniu zakodowanych danych użytkownika w nagłówku `Authorization` za pomocą podstawowego kodowania Base64. Jednak jest to metoda bezpieczeństwa o niskim poziomie, ponieważ dane są wysyłane w otwartej postaci i mogą być z łatwością przechwycone przez niepożądane osoby.

Alternatywą dla podstawowej autoryzacji może być np. protokół HTTPS, który zapewnia szyfrowanie danych przez użycie certyfikatów SSL. Istnieją także inne metody uwierzytelniania, takie jak np. autoryzacja oparta na tokenach.

## Zobacz także

- [Java Tutorials: Basic Authentication](https://www.baeldung.com/java-basic-authentication)
- [Java Doc: HttpURLConnection](https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html)
- [RFC 2617: Basic Authentication Scheme](https://tools.ietf.org/html/rfc2617)