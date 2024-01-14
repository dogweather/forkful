---
title:    "Java: Wyszukiwanie i zastępowanie tekstu"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Dlaczego warto wymieniać tekst w programowaniu Java?

Podczas programowania w języku Java bardzo często spotykamy się z koniecznością zmiany tekstu w kodzie źródłowym. Może to wynikać z różnych powodów, na przykład korekty błędów, aktualizacji danych czy zmiany formatu wyświetlanych informacji. W tym wpisie postaram się przybliżyć Wam technikę wyszukiwania i zamiany tekstu, która jest bardzo przydatna w codziennej pracy programisty.

## Jak to zrobić?

Zacznijmy od najbardziej podstawowej metody, czyli wykorzystania metody `replace()` z pakietu `String`. Przykład tego rozwiązania wyglądałby następująco:

```Java
String text = "Hello World!";
String changedText = text.replace("Hello", "Hi");

System.out.println(changedText); // Output: Hi World!
```

W tym przypadku, wykorzystując metodę `replace()`, zamieniamy wyraz "Hello" na "Hi". Proces ten jest bardzo prosty i przydatny szczególnie w przypadku pojedynczych zmian.

Kolejnym sposobem jest wykorzystanie wyrażeń regularnych. Jest to dużo bardziej zaawansowane podejście, ale umożliwia bardzo precyzyjne i elastyczne wyszukiwanie i zamianę tekstu. Przykład wykorzystania wyrażeń regularnych wyglądałby tak:

```Java
String text = "Hej 123 to jest liczba!";
String changedText = text.replaceAll("\\d+","replaced");

System.out.println(changedText); // Output: Hej replaced to jest liczba!
```

W tym przypadku zastosowaliśmy wyrażenie regularne, które oznacza, że wszystkie ciągi cyfr zostaną zastąpione ciągiem napisu "replaced". Dzięki temu możemy szybko i łatwo zastąpić różne formy danych w tekście, co może być bardzo przydatne przy eksportowaniu lub przetwarzaniu informacji.

## Głębszy zanurzenie

Podczas pracy z wyszukiwaniem i zamianą tekstu warto również pamiętać o wydajności i skuteczności naszego kodu. W przypadku dużych zbiorów danych lepszym rozwiązaniem może być użycie `StringBuilder`, który jest bardziej wydajny niż standardowy `String`. Przykład wykorzystania `StringBuilder` wyglądałby tak:

```Java
String text = "Ten tekst zostanie zamieniony za pomocą StringBuildera!";
StringBuilder sb = new StringBuilder(text);

sb.replace(0, 4, "To"); // Wymieniamy pierwsze słowo z "Ten" na "To"

System.out.println(sb.toString()); // Output: To tekst zostanie zamieniony za pomocą StringBuildera!
```

Dzięki użyciu `StringBuilder` zmieniliśmy bezpośrednio wartość tekstu, zamiast tworzenia nowej zmiennej, co sprawia, że nasz kod jest bardziej wydajny i oszczędniejszy dla pamięci.

## Zobacz także

Jeśli chcesz poszerzyć swoją wiedzę na temat zmiany tekstu w programowaniu Java, polecam zapoznanie się z poniższymi linkami:

- [Dokumentacja String](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Wyrażenia regularne w Javie](https://www.w3schools.com/java/java_regex.asp)
- [Porównanie wydajności Stringa i StringBuilder](https://www.baeldung.com/java-stringbuilder)

Mam nadzieję, że ten krótki wpis przybliżył Wam podstawy wyszukiwania i zamiany tekstu w języku Java. Dzięki temu narzędziu możemy szybko i sprawnie dokonać zmian w naszym kodzie, co znacznie ułatwia pracę programisty.