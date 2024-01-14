---
title:                "Java: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Dlaczego pobierać stronę internetową?

Pobieranie stron internetowych jest ważną umiejętnością w programowaniu webowym, ponieważ pozwala na pobieranie danych z innych stron i wykorzystanie ich w naszych projektach. Może to być szczególnie przydatne w budowie aplikacji internetowych, które wymagają dostępu do informacji z różnych źródeł.

# Jak to zrobić?

Aby pobrać stronę internetową w języku Java, możemy skorzystać z klasy URLConnection i BufferedReader. Należy najpierw utworzyć URL obiekt reprezentujący adres strony, a następnie użyć metody openConnection() aby pobrać obiekt URLConnection. Następnie tworzymy BufferedReader, który pozwala na odczytanie danych z tego połączenia i wypisanie ich na ekran.

```Java
URL url = new URL("https://www.przykladowastrona.pl");
URLConnection connection = url.openConnection();
BufferedReader reader = new BufferedReader(new InputStreamReader(connection.getInputStream()));
String inputLine;
while ((inputLine = reader.readLine()) != null) {
    System.out.println(inputLine);
}
reader.close();
```

Po wykonaniu powyższego kodu, powinniśmy zobaczyć zawartość pobranej strony internetowej w konsoli.

# Głębsze zagłębienie

Pobieranie stron internetowych może również wymagać użycia dodatkowych bibliotek w zależności od potrzeb. Na przykład, jeśli chcemy pobrać tylko pewne elementy strony, możemy skorzystać z biblioteki JSoup, która pozwala na parsowanie HTML i manipulowanie nim w łatwy sposób.

# Zobacz także
- [Jak pobierać dane z internetu w Javie](https://javastart.pl/baza-wiedzy/zacznij-programowac/pobieranie-danych-z-internetu-w-javie)
- [Oficjalna dokumentacja Java - klasa URLConnection](https://docs.oracle.com/javase/8/docs/api/java/net/URLConnection.html)
- [Biblioteka JSoup](https://jsoup.org/)