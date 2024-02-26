---
date: 2024-01-20 18:02:15.229784-07:00
description: "Wysy\u0142anie zapytania HTTP z podstawow\u0105 autentykacj\u0105 to\
  \ proces uwierzytelniania u\u017Cytkownika przez serwer przy u\u017Cyciu loginu\
  \ i has\u0142a zakodowanych w base64.\u2026"
lastmod: '2024-02-25T18:49:33.641694-07:00'
model: gpt-4-1106-preview
summary: "Wysy\u0142anie zapytania HTTP z podstawow\u0105 autentykacj\u0105 to proces\
  \ uwierzytelniania u\u017Cytkownika przez serwer przy u\u017Cyciu loginu i has\u0142\
  a zakodowanych w base64.\u2026"
title: "Wysy\u0142anie zapytania http z podstawow\u0105 autoryzacj\u0105"
---

{{< edit_this_page >}}

## Co i dlaczego?
Wysyłanie zapytania HTTP z podstawową autentykacją to proces uwierzytelniania użytkownika przez serwer przy użyciu loginu i hasła zakodowanych w base64. Programiści stosują tę metodę, by zapewnić bezpieczny dostęp do API lub zasobów webowych.

## Jak to zrobić:
```java
import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.Base64;

public class BasicAuthRequest {

    public static void main(String[] args) throws IOException {
        String apiUrl = "http://example.com/api/data"; // Zamień na odpowiedni URL
        String username = "user";
        String password = "pass";

        URL url = new URL(apiUrl);
        HttpURLConnection connection = (HttpURLConnection) url.openConnection();

        // Dodajemy nagłówek z autentykacją podstawową
        String encoded = Base64.getEncoder().encodeToString((username + ":" + password).getBytes());
        connection.setRequestProperty("Authorization", "Basic " + encoded);

        // Odpowiedź serwera
        int responseCode = connection.getResponseCode();
        System.out.println("Response Code: " + responseCode);

        // Tu wstaw obsługę strumienia danych z odpowiedzi...
    }
}
```
Wynik:
```
Response Code: 200
```

## Szersza perspektywa
HTTP Basic Authentication jest prostym mechanizmem kontroli dostępu do zasobów sieciowych. Wprowadzony w 1996 roku w HTTP/1.0, do dziś jest powszechnie stosowany ze względu na swoją prostotę. Alternatywami są bardziej złożone metody jak OAuth, tokeny JWT czy systemy oparte na certyfikatach SSL/TLS. Implementując podstawową autentykację, ważne jest użycie połączenia szyfrowanego (HTTPS), aby zapobiec potencjalnemu przechwyceniu danych uwierzytelniających.

## Zobacz również
- [Specyfikacja HTTP Basic Authentication](https://tools.ietf.org/html/rfc7617)
- [Przewodnik po uwierzytelnianiu w Javie](https://www.baeldung.com/java-http-request)
