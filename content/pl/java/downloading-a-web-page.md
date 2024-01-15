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

## Dlaczego

Pobieranie stron internetowych jest niezbędnym narzędziem w świecie programowania. Dzięki temu możemy automatycznie pobierać dane, analizować je i wykorzystywać w naszych projektach.

## Jak to zrobić

Aby pobrać stronę internetową w Java, musimy użyć klasy `URL`. Najpierw musimy ją zaimportować:

```Java
import java.net.URL;
```

Następnie, tworzymy nowy obiekt `URL` podając jako argument adres URL strony, którą chcemy pobrać:

```Java
URL url = new URL("https://example.com");
```

Teraz musimy utworzyć połączenie z tą stroną, używając metody `openConnection()`:

```Java
URLConnection connection = url.openConnection();
```

Aby pobrać zawartość strony, musimy użyć obiektu `InputStream` oraz metody `openStream()`:

```Java
InputStream stream = connection.getInputStream();
```

Mając już dostęp do strumienia danych, możemy je odczytywać. Przykładowo, możemy wyświetlić zawartość strony w konsoli:

```Java
int data = stream.read();
while(data != -1) { // dopóki nie będzie koniec strumienia
    System.out.print((char) data); // wyświetlamy kolejne znaki
    data = stream.read(); // wczytujemy kolejne dane
}
```

Kod powyżej odczytuje plik po bajcie i wyświetla go w konsoli, aż do osiągnięcia końca strumienia (oznaczanego przez wartość -1). Możemy także odczytywać dane w inny sposób, na przykład wiersz po wierszu:

```Java
BufferedReader reader = new BufferedReader(new InputStreamReader(stream));
String line = reader.readLine(); // czytamy pierwszy wiersz
while(line != null) { // dopóki nie będziemy mieć końca pliku
    System.out.println(line); // wypisujemy wiersz na konsoli
    line = reader.readLine(); // czytamy kolejny wiersz
}
```

## Dogłębna analiza

Powyższe przykłady pokazują sposób prosty na pobieranie stron internetowych, ale wciąż istnieją pewne problemy związane z tą metodą. Po pierwsze, nie wszystkie strony mogą być pobierane w ten sam sposób - mogą one wymagać ustawienia nagłówków lub autoryzacji. Po drugie, nie wszystkie strony są statyczne - niektóre używają JavaScriptu do generowania zawartości w przeglądarce, więc nie da się jej pobrać w ten sam sposób.

Istnieją różne biblioteki do pobierania stron internetowych w Java, takie jak JSoup czy HttpComponents. Poprzez wykorzystanie takiej biblioteki, upraszczamy proces pobierania stron i nie musimy przejmować się takimi problemami jak wyżej wymienione.

## Zobacz też

- [Java.net.URL](https://docs.oracle.com/javase/10/docs/api/java/net/URL.html)
- [Java.net.URLConnection](https://docs.oracle.com/javase/10/docs/api/java/net/URLConnection.html)
- [JSoup](https://jsoup.org/)
- [HttpComponents](https://hc.apache.org/index.html)