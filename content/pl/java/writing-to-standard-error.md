---
title:    "Java: Zapisywanie do standardowego błędu"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego
Pisanie do standardowego wyjścia błędów jest nieodłączną częścią programowania w języku Java. Jest to wyjątkowo przydatne narzędzie, które pomaga oznaczać i debugować błędy w naszym kodzie. W tym artykule dowiesz się, dlaczego warto używać standardowego wyjścia błędów i jak to zrobić.

## Jak to zrobić
Pisanie do standardowego wyjścia błędów jest bardzo proste w języku Java. Wystarczy użyć metody `System.err.println()` lub `System.err.print()`. Przykłady kodu poniżej pokazują, jak użyć tych metod:

```Java
System.err.println("To jest błąd!");   // wypisze: To jest błąd! w standardowym wyjściu błędów
System.err.print("To jest ");   // nie przejdzie do nowej linii
System.err.println("błąd!");   // wypisze: To jest błąd! w standardowym wyjściu błędów
```

Można również użyć standardowego strumienia błędów jako argumentu w metodzie `println()` lub `print()`:

```Java
int liczba = 0;
if (liczba == 0) {
    System.err.println("Liczba nie może być równa 0!");   // wypisze: Liczba nie może być równa 0!
}
```

## Deep Dive
Główną różnicą między standardowym wyjściem a standardowym wyjściem błędów jest przeznaczenie. Standardowe wyjście jest używane do normalnych wyjść przekazywanych użytkownikowi, podczas gdy standardowe wyjście błędów służy do raportowania błędów i informacji diagnostycznych. Dzięki temu programista może łatwo rozpoznać, gdzie znajduje się problem w kodzie i naprawić go.

Istnieje również możliwość przekierowania standardowego wyjścia błędów do pliku lub strumienia wyjściowego. Można to zrobić za pomocą klasy `PrintStream` i metody `setErr()`:

```Java
PrintStream strumienBledow = new PrintStream(new File("bledy.txt"));
System.setErr(strumienBledow);
System.err.println("Ten błąd zostanie zapisany do pliku bledy.txt!");   // zapisze błąd do wskazanego pliku
```

## Zobacz również
- [Java: Writing to Standard Error](https://www.javatpoint.com/java-system-err)
- [Java System Properties](https://docs.oracle.com/javase/tutorial/essential/environment/sysprop.html)
- [Java: Difference between System.out and System.err](https://www.baeldung.com/java-system-out-err)