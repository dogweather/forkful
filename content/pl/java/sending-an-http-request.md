---
title:                "Wysyłanie żądania http"
html_title:           "Java: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Dlaczego

Wysyłanie żądania HTTP jest niezbędnym elementem pracy wielu aplikacji, szczególnie tych, które komunikują się z serwerem. Pozwala ono na pobieranie i wysyłanie danych, co jest kluczowe w budowaniu funkcjonalnych i interaktywnych aplikacji internetowych.

## Jak To Zrobić

Wysłanie żądania HTTP w języku Java jest proste i wymaga użycia odpowiedniej biblioteki, np. java.net.URLConnection. Poniższy kod pokazuje przykładową implementację wysyłania żądania GET wraz z obsługą potencjalnych wyjątków:

```Java
import java.io.*;
import java.net.*;

public class HttpRequestExample {

    public static void main(String[] args) {
        try {
            // utworzenie połączenia
            URL url = new URL("https://example.com");
            HttpURLConnection con = (HttpURLConnection) url.openConnection();
            con.setRequestMethod("GET");

            // odczytanie odpowiedzi
            BufferedReader in = new BufferedReader(new InputStreamReader(con.getInputStream()));
            String inputLine;
            StringBuffer response = new StringBuffer();
            while ((inputLine = in.readLine()) != null) {
                response.append(inputLine);
            }

            // wypisanie odpowiedzi
            System.out.println(response.toString());

            // zamknięcie połączenia
            in.close();
            con.disconnect();
        } catch (MalformedURLException e) {
            System.out.println("Błędny adres URL: " + e.getMessage());
        } catch (IOException e) {
            System.out.println("Błąd podczas odczytywania danych: " + e.getMessage());
        }
    }
}
```
```
W powyższym przykładzie, na początku tworzymy obiekt `URL` z adresem, do którego chcemy wysłać żądanie. Następnie wykorzystujemy funkcję `openConnection()` do utworzenia obiektu `HttpURLConnection`, który odpowiada za przeprowadzenie żądania. Wykorzystując metodę `setRequestMethod()`, ustawiamy żądany typ żądania (w tym przypadku GET). W kolejnych linijkach kodu odczytujemy odpowiedź i zamieszczamy ją w `StringBuffer` oraz wypisujemy ją na ekranie. Na końcu zamykamy połączenie i rozłączamy. 

## Deep Dive

Istnieje wiele skomplikowanych procesów, które mogą wystąpić podczas wysyłania żądania HTTP, takich jak przekierowania, obsługa błędów lub zabezpieczenia. Przeprowadzając "głębszą" analizę tego procesu, możemy lepiej zrozumieć, jakśmy komunikują się z serwerem oraz jakie są możliwe rezultaty naszych żądań. Dzięki temu będziemy lepiej przygotowani na ewentualne problemy oraz będziemy w stanie zoptymalizować naszą komunikację z serwerem.

## Zobacz też

- [Dokumentacja Java.net.URLConnection](https://docs.oracle.com/en/java/javase/11/docs/api/java.net/java/net/URLConnection.html)
- [Tutorial Wysyłania Http Request w Javie](https://www.baeldung.com/java-http-request)
- [Podstawy Protokołu HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP)