---
date: 2024-01-20 17:44:19.501168-07:00
description: "Pobieranie strony internetowej to proces, dzi\u0119ki kt\xF3remu mo\u017C\
  emy uzyska\u0107 jej zawarto\u015B\u0107 w formacie tekstowym. Programi\u015Bci\
  \ robi\u0105 to, by analizowa\u0107 dane,\u2026"
lastmod: '2024-02-25T18:49:33.640772-07:00'
model: gpt-4-1106-preview
summary: "Pobieranie strony internetowej to proces, dzi\u0119ki kt\xF3remu mo\u017C\
  emy uzyska\u0107 jej zawarto\u015B\u0107 w formacie tekstowym. Programi\u015Bci\
  \ robi\u0105 to, by analizowa\u0107 dane,\u2026"
title: Pobieranie strony internetowej
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
