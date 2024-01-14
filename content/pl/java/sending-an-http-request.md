---
title:                "Java: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach wszystkie aplikacje internetowe muszą mieć możliwość komunikacji z serwerami poprzez protokół HTTP. Dlatego nie ma nic dziwnego w tym, że większość programistów w Javie musi poznać i umieć wysyłać żądania HTTP. Nauka jak to zrobić może być przydatna, gdy tworzysz swoją własną aplikację lub gdy musisz zintegrować się z istniejącym serwisem internetowym.

## Jak to zrobić

Do wysyłania żądań HTTP w Javie potrzebne są nam dwie główne biblioteki: `HttpURLConnection` i `URL`. Najpierw deklarujemy obiekt `URL` z adresem, do którego chcemy się połączyć. Następnie wywołujemy na nim metodę `openConnection()`, co zwraca obiekt `HttpURLConnection`. Teraz możemy ustawić żądanie HTTP za pomocą metod `setRequestMethod()` i `setRequestProperty()` oraz wysłać żądanie używając `getInputStream()` lub `getOutputStream()`, w zależności od rodzaju żądania. Poniżej znajduje się przykładowy kod dla metody GET:

```
URL url = new URL("https://www.example.com/api");
HttpURLConnection connection = (HttpURLConnection) url.openConnection();
connection.setRequestMethod("GET");

int responseCode = connection.getResponseCode();
BufferedInputStream inputStream = new BufferedInputStream(connection.getInputStream());
String response = new String(inputStream.readAllBytes());
System.out.println("Response code: " + responseCode);
System.out.println("Response body: " + response);
```

Jeśli wszystko przebiegnie pomyślnie, otrzymamy odpowiedź w postaci kodu odpowiedzi i treści odpowiedzi.

## Dogłębna analiza

Wysyłanie żądań HTTP może być bardziej skomplikowane niż tylko użycie powyższego kodu. Musimy mieć na uwadze różne metody, takie jak GET, POST, PUT, DELETE, a także różne nagłówki i parametry. W przypadku niektórych serwerów może być konieczna uwierzytelnienie za pomocą tokena lub hasła. W takich przypadkach będziemy musieli użyć metody `setRequestProperty()` do ustawienia odpowiedniego nagłówka. Możemy również zmodyfikować nasze żądanie HTTP za pomocą metody `setDoOutput()` i `setDoInput()`. Wśród wielu opcji, kóre należy uwzględnić przy wysyłaniu żądań HTTP, najważniejszą jest zrozumienie protokołu i dokumentacji dla wybranej biblioteki, którą będziemy wykorzystywać.

## Zobacz też

- [Dokumentacja biblioteki HttpURLConnection w Javie](https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html)
- [Dokumentacja biblioteki URL w Javie](https://docs.oracle.com/javase/8/docs/api/java/net/URL.html)
- [Przykłady wysyłania żądań HTTP w Javie](https://www.baeldung.com/java-http-request)