---
title:                "Wysyłanie zapytania http z podstawową autoryzacją"
aliases: - /pl/java/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:02:15.229784-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wysyłanie zapytania http z podstawową autoryzacją"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/sending-an-http-request-with-basic-authentication.md"
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
