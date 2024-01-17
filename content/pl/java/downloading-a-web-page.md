---
title:                "Pobieranie strony internetowej"
html_title:           "Java: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Co to jest i dlaczego?

Pobieranie stron internetowych to proces, w którym program pobiera zawartość strony internetowej z serwera internetowego i zapisuje ją w formie pliku na lokalnym urządzeniu komputerowym. Programiści często wykonują ten proces w celu przetwarzania danych lub wykorzystania ich do dalszych działań.

## Jak to zrobić:

```java
import java.net.URL;
import java.io.BufferedInputStream;
import java.io.FileOutputStream;

public class PobierzStrone {
    public static void main(String[] args) throws Exception {
        // utwórz obiekt URL do strony internetowej
        URL url = new URL("https://www.example.com");
        // otwórz połączenie z serwerem i pobierz dane
        BufferedInputStream bis = new BufferedInputStream(url.openStream());
        // zapisz pobrane dane do pliku
        FileOutputStream fis = new FileOutputStream("strona.html");
        byte[] buffer = new byte[1024];
        int count;
        while ((count = bis.read(buffer)) != -1) {
            fis.write(buffer, 0, count);
        }
        // zamknij strumienie danych
        fis.close();
        bis.close();
        // poinformuj o poprawnym pobraniu strony
        System.out.println("Strona została pobrana pomyślnie!");
    }
}
```

## Głębszy zanurzenie:

Pobieranie stron internetowych było wykorzystywane od dawna w celu tworzenia katalogów stron, wyszukiwarek internetowych i innych narzędzi do przetwarzania danych z internetu. Alternatywnie, można również użyć biblioteki JSoup lub HttpClient do pobrania stron internetowych z wykorzystaniem metod HTTP. Implementacja pobierania stron internetowych może wymagać również uwierzytelniania lub ustawienia nagłówków żądania w przypadku stron wymagających uwierzytelnienia lub konkretnych ustawień.

## Zobacz także:

- [Biblioteka JSoup](https://jsoup.org/)
- [Apache HttpClient](http://hc.apache.org/httpcomponents-client-ga/)
- [Pobieranie stron internetowych w Javie przy użyciu biblioteki JSoup](https://www.baeldung.com/java-httprequest)