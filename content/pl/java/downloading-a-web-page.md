---
title:                "Pobieranie strony internetowej"
html_title:           "C#: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Pobieranie strony internetowej to proces zapisywania jej zawartości lokalnie na naszym komputerze. Programiści robią to, aby analizować zawartość strony, przetwarzać dane, monitorować zmiany i tworzyć aplikacje web scraping.

## Jak to zrobić:

Użyjemy klasy `java.net.URL` i `java.nio.file`. Oto jak to zrobisz:

```Java
import java.io.InputStream;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Paths;

public class WebDownloader {
    public static void main(String[] args) throws Exception {
        URL website = new URL("http://www.example.com");
        try (InputStream in = website.openStream()) {
            Files.copy(in, Paths.get("downloaded.html"));
        }
    }
}
```

Gdy uruchomisz ten program, pobierze on stronę www.example.com i zapisze ją jako "downloaded.html" w katalogu projektu.

## Głębsze spojrzenie:

1. Historyczny kontekst: Początkowo różne biblioteki, takie jak Apache HttpClient, były używane do pobierania stron internetowych w Javie. Ale od Java 7, możemy wykorzystać wbudowane API, takie jak `java.net.URL`.

2. Alternatywy: Inne biblioteki, takie jak Jsoup lub HtmlUnit, oferują bardziej zaawansowane narzędzia do przetwarzania pobranych stron.

3. Szczegóły implementacyjne: Powyższy kod otwiera połączenie do URL, tworzy strumień wejściowy, zapisuje zawartość strony do pliku za pośrednictwem `Files.copy()`. Pamiętaj, aby zawsze zamykać strumienie wejściowe, co tutaj gwarantuje konstrukcja try-with-resources.

## Zobacz również:

1. Dokumentacja Java `java.net.URL`: https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/net/URL.html

2. Przewodnik po składnikach `java.nio`: https://docs.oracle.com/javase/tutorial/essential/io/fileio.html

3. Biblioteka Jsoup: https://jsoup.org/