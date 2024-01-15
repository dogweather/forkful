---
title:                "Odczytywanie argumentów wiersza poleceń"
html_title:           "Java: Odczytywanie argumentów wiersza poleceń"
simple_title:         "Odczytywanie argumentów wiersza poleceń"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą Java i chcesz nauczyć się, jak czytać argumenty wiersza poleceń, to jesteś we właściwym miejscu. W tym artykule dowiesz się, dlaczego warto poznać tę umiejętność i jak ją wykorzystać w swoich projektach.

## Jak to zrobić

Czytanie argumentów wiersza poleceń w Javie jest prostym procesem, który wymaga tylko kilku linijek kodu. Najpierw musimy utworzyć obiekt klasy `Scanner`, który będzie czytał dane z wiersza poleceń. Następnie używając metody `next()` możemy pobrać kolejny argument i przypisać go do zmiennej. Poniższy kod będzie czytał argumenty wiersza poleceń i wyświetlał je na ekranie:

```Java
import java.util.Scanner;

public class CommandLineArguments {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        while (scanner.hasNext()) {
            String argument = scanner.next();
            System.out.println("Argument: " + argument);
        }
    }
}
```

Jeśli uruchomisz ten kod z kilkoma argumentami, na przykład `java CommandLineArguments arg1 arg2 arg3`, to otrzymasz następujący wynik:

```
Argument: arg1
Argument: arg2
Argument: arg3
```

## Głębszy zanurzenie

Czytanie argumentów wiersza poleceń może być przydatne w różnych scenariuszach, na przykład w przypadku gdy potrzebujemy przekazać pewne informacje do naszego programu przed jego uruchomieniem. Możemy również wykorzystać argumenty wiersza poleceń do przypisywania wartości do zmiennych lub uruchamiania różnych funkcjonalności w naszym programie.

Warto również pamiętać, że argumenty wiersza poleceń są przekazywane do programu jako napisy, więc w niektórych przypadkach może być konieczne przekonwertowanie ich na odpowiednie typy danych.

## Zobacz również

Jeśli interesuje Cię więcej na temat pracy z argumentami wiersza poleceń w Javie, polecam zapoznać się z poniższymi artykułami:

- https://www.baeldung.com/java-command-line-arguments
- https://www.tutorialspoint.com/java/java_command_line_arguments.htm
- https://www.geeksforgeeks.org/command-line-arguments-in-java/

Teraz już wiesz, jak czytać argumenty wiersza poleceń w Javie, więc możesz wykorzystać tę umiejętność w swoich projektach. Powodzenia!