---
title:                "Pobieranie strony internetowej"
aliases:
- /pl/java/downloading-a-web-page/
date:                  2024-01-20T17:44:19.501168-07:00
model:                 gpt-4-1106-preview
simple_title:         "Pobieranie strony internetowej"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?
Pobieranie strony internetowej to proces, dzięki któremu możemy uzyskać jej zawartość w formacie tekstowym. Programiści robią to, by analizować dane, monitorować zmiany lub wydobywać informacje automatycznie.

## How to:
W Java można to zrobić za pomocą `java.net.HttpURLConnection` albo popularnych bibliotek takich jak `Jsoup`. Oto prosty przykład użycia `HttpURLConnection`:

```java
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;

public class WebPageDownloader {
    public static void main(String[] args) throws IOException {
        String urlString = "http://example.com";
        URL url = new URL(urlString);
        HttpURLConnection connection = (HttpURLConnection) url.openConnection();

        try (BufferedReader reader = new BufferedReader(
                new InputStreamReader(connection.getInputStream()))) {
            String inputLine;
            while ((inputLine = reader.readLine()) != null) {
                System.out.println(inputLine);
            }
        } finally {
            connection.disconnect();
        }
    }
}
```

Output będzie zawierał HTML pobranej strony.

## Deep Dive:
Pobieranie stron internetowych w Javie ma długą historię. Wcześniejsze wersje Javy korzystały z `URLConnection`, ale w praktyce częściej używa się `HttpURLConnection`, która obsługuje specyfikę HTTP. Alternatywą jest biblioteka Apache HttpClient czy wspomniane `Jsoup`, które oferują więcej funkcji i łatwość użycia. Apache HttpClient jest robustnym rozwianiem, odpowiednim do skomplikowanych zadań. Z kolei `Jsoup` jest idealny do parsowania HTML, co sprawia, że jest wygodny w ekstrakcji konkretnych danych ze stron.

## See Also:
- [Jsoup Official Site](https://jsoup.org/)
- [Oracle's Java Networking Tutorial](https://docs.oracle.com/javase/tutorial/networking/urls/index.html)
