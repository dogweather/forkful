---
date: 2024-01-20 17:44:19.501168-07:00
description: "How to: W Java mo\u017Cna to zrobi\u0107 za pomoc\u0105 `java.net.HttpURLConnection`\
  \ albo popularnych bibliotek takich jak `Jsoup`. Oto prosty przyk\u0142ad u\u017C\
  ycia\u2026"
lastmod: '2024-03-13T22:44:35.276178-06:00'
model: gpt-4-1106-preview
summary: "W Java mo\u017Cna to zrobi\u0107 za pomoc\u0105 `java.net.HttpURLConnection`\
  \ albo popularnych bibliotek takich jak `Jsoup`."
title: Pobieranie strony internetowej
weight: 42
---

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
