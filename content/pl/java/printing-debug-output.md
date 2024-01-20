---
title:                "Drukowanie komunikatów debugowania"
html_title:           "Haskell: Drukowanie komunikatów debugowania"
simple_title:         "Drukowanie komunikatów debugowania"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wyświetlanie informacji debugujących (debug output) to technika wykorzystywana przez programistów do śledzenia i rozwiązywania problemów z kodem. Ułatwia to zrozumienie, co dzieje się wewnątrz programu podczas jego działania.

## Jak to zrobić:

Chcesz wydrukować wiadomość debugowania w Javie? To jest bardzo proste. Wykorzystaj standardowe wyjście System.out lub wyjście błędów System.err. Przykład:

```Java
public class Main {
    public static void main(String[] args) {
        System.out.println("Debugging Started");
        int a = 10;
        int b = 0;
        try {
            int result = a / b;
            System.out.println("Result: " + result);
        } catch (ArithmeticException e) {
            System.err.println("Błąd! Dzielisz przez zero");
        }
        System.out.println("Debugging Finished");
    }
}
```
W wyniku tego otrzymasz:

```
Debugging Started
Błąd! Dzielisz przez zero
Debugging Finished
```

## Głębsza analiza:

- **Historyczne kontekst**: W początkach Javy, programiści często korzystali z instrukcji wydruku do śledzenia i debugowania swojego kodu. Od tego czasu nic się nie zmieniło!

- **Alternatywy**: Chociaż System.out i System.err to najprostsze sposoby na wydrukowanie informacji debugujących, są inne możliwości, takie jak wykorzystanie loggerów (np. Logger od SLF4J lub Log4J), które umożliwiają bardziej rozbudowane i konfigurowalne logowanie.

- **Szczegóły implementacji**: System.out i System.err to statyczne finalne zmienne, które są instancjami klasy PrintStream. Ta klasa zawiera metody pozwalające na zapisanie różnych typów danych, w tym liczby całkowitej, liczby zmiennoprzecinkowej, znaku i łańcucha.

## Zobacz także:

- [Logging w Javie](https://docs.oracle.com/javase/8/docs/technotes/guides/logging/overview.html)
- [Logger od SLF4J](http://www.slf4j.org/manual.html)
- [Logger od Log4J](https://logging.apache.org/log4j/2.x/manual/introduction.html)