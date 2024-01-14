---
title:                "Java: Wyciąganie podciągów"
simple_title:         "Wyciąganie podciągów"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/extracting-substrings.md"
---

{{< edit_this_page >}}

# Dlaczego warto wyciągać podciągi

Wyciąganie podciągów jest ważnym aspektem programowania w języku Java, ponieważ pozwala na wygodniejsze i efektywniejsze manipulowanie tekstem. Jeśli pracujesz nad projektami, w których musisz operować na wielu różnych ciągach znaków, umiejętność wyciągania podciągów będzie niezbędna. Jest to również ważne w kontekście optymalizacji kodu, ponieważ pozwala na uniknięcie niepotrzebnego przekazywania dużych ciągów znaków pomiędzy funkcjami.

## Jak wyciągać podciągi

Aby wyciągać podciągi, możesz wykorzystać metodę `substring()` klasy `String`. Przyjmuje ona dwa argumenty: początkowy i końcowy indeks podciągu. Pamiętaj, że indeksowanie w języku Java zaczyna się od zera, więc pierwszy znak będzie miał indeks 0.

```Java
String sentence = "To jest przykładowe zdanie.";
System.out.println(sentence.substring(3, 13));
```

**Output:** jest przykł

Jak widać w powyższym przykładzie, wyciągnęliśmy podciąg z oryginalnego zdania, zaczynając od 4. znaku (indeks 3) do 14. znaku (indeks 13).

Możesz również wykorzystać metodę `substring()` do wydobycia podciągu od danego indeksu do końca ciągu, podając tylko pierwszy argument.

```Java
String name = "Katarzyna";
System.out.println(name.substring(3));
```

**Output:** arzyna

## Pogłębione informacje o wyciąganiu podciągów

Wyciąganie podciągów jest możliwe również przy użyciu klasy `StringBuilder`. Jest to bardziej wydajna metoda, jeśli potrzebujesz modyfikować duże ciągi znaków. Metoda `substring()` jest dostępna również w tej klasie, ale należy pamiętać, że biblioteka ta indeksuje znaki od 1, a nie od 0.

Warto również zwrócić uwagę na fakt, że metoda `substring()` nie zmienia oryginalnego ciągu znaków, tylko zwraca nowy ciąg zawierający wyciągnięty podciąg. Jeśli chcesz wprowadzić zmiany bezpośrednio do oryginalnego ciągu, możesz wykorzystać metody klasy `StringBuilder`, takie jak `replace()` lub `insert()`.

# Zobacz również

- Oficjalna dokumentacja Javy dla klasy `String` i `StringBuilder`: https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/lang/String.html
- Przykładowe ćwiczenia z wykorzystaniem wyciągania podciągów: https://www.w3resource.com/java-exercises/string/index.php