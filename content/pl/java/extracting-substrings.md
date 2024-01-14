---
title:    "Java: Wydobywanie podciągów"
keywords: ["Java"]
---

{{< edit_this_page >}}

# Dlaczego warto wyciągnąć podrzędną ciąg znaków?

Wyciąganie podrzędnych ciągów znaków jest bardzo użyteczną umiejętnością dla każdego programisty Java. Dzięki tej funkcji, możemy łatwo manipulować i przetwarzać tekst, co jest niezbędne w wielu aplikacjach. W tym artykule opowiemy o tym jak wyciągać podrzędne ciągi znaków oraz pokażemy kilka praktycznych przykładów.

## Jak to zrobić?

Aby wyciągnąć podrzędny ciąg znaków w Java, musimy wykorzystać metodę `substring()` z klasy `String`. Metoda ta przyjmuje dwa argumenty: indeks początkowy oraz indeks końcowy. Indeks początkowy oznacza od którego znaku chcemy zacząć wyciąganie, a indeks końcowy oznacza którym znakiem chcemy zakończyć.

```Java
String tekst = "To jest przykładowy tekst";

// Wyciągamy podciąg znaków od indeksu 7 do 18 (nie włącznie)
String podciag = tekst.substring(7, 18);

System.out.println(podciag); // "jest przykładowy"
```

Pamiętajmy, że indeks pierwszego znaku w ciągu znaków jest równy 0, więc jeśli chcemy wyciągnąć cały tekst zaczynając od trzeciego znaku, musimy użyć indeksu 2 jako argumentu metody `substring()`.

Możemy również wykorzystać metodę `substring()` w połączeniu z innymi metodami klasy `String`, na przykład `length()`. Dzięki temu, możemy dynamicznie wyciągać podciągi znaków nie znając dokładnie ich długości.

```Java
String tekst = "Wyciąganie podciągów znaków jest łatwe";

// Wyciągamy podciąg od indeksu 13 do końca
String podciag = tekst.substring(13, tekst.length());

System.out.println(podciag); // "podciągów znaków jest łatwe"
```

## Deep Dive

Wyciąganie podrzędnych ciągów znaków napotyka się na dwa główne problemy: obsłużenie wyjątków oraz wybór odpowiednich indeksów. Aby uniknąć błędów związanych z indeksowaniem, zawsze możemy sprawdzić długość tekstu i dostosować indeksy odpowiednio. Natomiast obsługa wyjątków może być użyteczna, jeśli nie jesteśmy pewni czy nasz tekst zawiera wystarczającą liczbę znaków dla naszego podciągu.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o metodzie `substring()` oraz manipulowaniu tekstem w ogóle, polecamy zapoznać się z poniższymi linkami:

- Oficjalna dokumentacja Java o metodzie `substring()` - [https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#substring-int-int-](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#substring-int-int-)
- Wideo tutorial o manipulowaniu tekstem w Java - [https://www.youtube.com/watch?v=PILOy0xFCuM](https://www.youtube.com/watch?v=PILOy0xFCuM)
- Przewodnik po manipulowaniu tekstem w Java na przykładach - [https://www.baeldung.com/string-java](https://www.baeldung.com/string-java)