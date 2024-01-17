---
title:                "Pisanie do standardowego błędu"
html_title:           "Java: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Co & Dlaczego?
Zapisywanie do standardowego wyjścia błędów to sposób na wyświetlanie komunikatów o błędach lub ostrzeżeń dla użytkownika podczas działania programu. Programiści używają go, aby pomóc w debugowaniu i poprawie działania aplikacji.

# Jak to zrobić:
Java dostarcza nam mechanizm ```System.err.println()```, który pozwala na wypisywanie tekstu do standardowego wyjścia błędów. Przykład użycia:

```Java
System.err.println("Błąd: Nie można odnaleźć pliku!");
```

To spowoduje wyświetlenie komunikatu "Błąd: Nie można odnaleźć pliku!" na konsoli programu. Możemy również przekazać do tej metody wyjątek, który zostanie wyświetlony wraz z odpowiednim komunikatem:

```Java
catch (FileNotFoundException e) {
    System.err.println("Błąd: Nie można odnaleźć pliku!");
    e.printStackTrace();
}
```

# Głęboka Zanurzona:
Standardowe wyjście błędów zostało zaprojektowane w celu ułatwienia programistom w znajdowaniu i rozwiązywaniu błędów w aplikacji. Dzięki temu mechanizmowi możliwe jest wyświetlanie i przekazywanie informacji o błędach do użytkownika, co ułatwia w ich zrozumieniu i naprawieniu. Alternatywą dla tego mechanizmu jest standardowe wyjście, które jest wykorzystywane do wyświetlania informacji do użytkownika, ale nie zawiera informacji o błędach.

W Javie standardowe wyjście błędów jest domyślnie przekierowywane na konsolę, ale istnieje możliwość zmiany tej konfiguracji. Można to zrobić za pomocą klasy ```System.setErr()```, która pozwala na ustawienie innego strumienia wyjścia niż konsola. 

# Zobacz również:
- Dokumentacja Java: https://docs.oracle.com/javase/8/docs/api/java/lang/System.html#err
- Wideo tutorial: https://www.youtube.com/watch?v=6777R1L6x9s
- Inne artykuły o programowaniu w Javie: https://sutherland-hdc.github.io/articles.html