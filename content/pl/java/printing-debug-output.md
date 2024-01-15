---
title:                "Wypisywanie wyjścia debugowania"
html_title:           "Java: Wypisywanie wyjścia debugowania"
simple_title:         "Wypisywanie wyjścia debugowania"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

*Dlaczego warto wypisywać informacje debugowania w kodzie Java?* Wypisywanie informacji debugowania jest pomocne podczas wychwytywania błędów i śledzenia przepływu danych w trakcie działania programu. Dzięki temu można łatwiej zlokalizować problemy w kodzie i szybciej je naprawić.

## Jak to zrobić

W Java istnieje kilka sposobów na wypisywanie informacji debugowania. Jednym z nich jest użycie metody `System.out.println()`, która wypisuje podane argumenty na standardowym wyjściu. Przykład użycia:

```Java
String imie = "Katarzyna";
System.out.println("Witaj, " + imie + "!");
```
*Wynik:*
```
Witaj, Katarzyna!
```

Innym sposobem jest użycie klasy `Logger` z pakietu `java.util.logging`. Przykład użycia:

```Java
import java.util.logging.Logger;

public class Main {
    public static void main(String[] args) {
        Logger log = Logger.getLogger(Main.class.getName());
        log.info("To jest informacja debugowania.");
    }
}
```
*Wynik w konsoli:*
```
INFO: To jest informacja debugowania.
```

## Deep Dive

Wypisywanie informacji debugowania może być uzupełnione o dodatkowe funkcjonalności, takie jak wypisywanie do plików lub filtrowanie poziomów ważności informacji. Dla bardziej zaawansowanych użytkowników polecam przeczytać dokumentację klasy `Logger` oraz inne narzędzia i biblioteki dedykowane do debugowania w Java.

## Zobacz też

- Dokumentacja klasy [Logger](https://docs.oracle.com/javase/8/docs/api/java/util/logging/Logger.html)
- [Jak debugować swoją aplikację Java w IntelliJ](https://www.jetbrains.com/help/idea/debugging-your-first-java-application.html)
- Narzędzia do debugowania w Java, takie jak [Eclipse Debugger](https://www.eclipse.org/eclipse/news/4.6/jdt.php#debugger)