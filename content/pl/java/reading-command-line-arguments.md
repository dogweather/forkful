---
title:                "Java: Odczytywanie argumentów wiersza poleceń"
simple_title:         "Odczytywanie argumentów wiersza poleceń"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś nowym programistą lub starym wyjadaczem, prawdopodobnie słyszałeś o "argumentach wiersza poleceń" lub "parametrach wiersza poleceń". Ale czy wiesz, dlaczego są one tak ważne w programowaniu w języku Java? Czy warto poświęcić czas na naukę ich działania? W tym wpisie dowiesz się, dlaczego warto poznać obsługę argumentów wiersza poleceń w Java i jak może to ułatwić Twoją pracę.

## Jak to zrobić

Obsługa argumentów wiersza poleceń w Java jest prosta i przydatna. Możesz uzyskać dostęp do nich dzięki obiektowi typu `String[]`, który jest przekazywany do metody `main()` Twojego programu. Aby wyświetlić argumenty wiersza poleceń, wystarczy użyć pętli `for-each` i metody `System.out.println()`, np.

```Java
public static void main(String[] args) {
    for (String arg : args) {
        System.out.println(arg);
    }
}
```

Jeśli uruchomisz ten program, podając mu argumenty wiersza poleceń, na przykład `java MyApp argument1 argument2`, to otrzymasz na wyjściu:

```
argument1
argument2
```

Jednak argumenty wiersza poleceń mogą być również używane do przekazywania parametrów do Twojego programu. Aby to zrobić, musisz najpierw określić, ile parametrów chcesz przekazać i jakie są ich typy. Następnie możesz je odczytać z obiektu `args` i przekonwertować na odpowiedni typ danych, jak w poniższym przykładzie:

```Java
public static void main(String[] args) {
    int number = Integer.parseInt(args[0]);
    String text = args[1];
    System.out.println("Liczba: " + number);
    System.out.println("Tekst: " + text);
}
```

Jeśli uruchomisz powyższy program z argumentami `10 hello`, to otrzymasz na wyjściu:

```
Liczba: 10
Tekst: hello
```

## Deep Dive

Teraz, gdy już wiesz, jak odczytać i wykorzystać argumenty wiersza poleceń w swoim programie Java, zastanówmy się, dlaczego są one tak ważne. Po pierwsze, dzięki nim możesz zmieniać zachowanie swojego programu bez konieczności zmieniania samego kodu. Możesz również użyć argumentów wiersza poleceń do przekazywania ustawień lub parametrów do swojego programu, co pozwala na większą kontrolę nad jego działaniem. Ponadto, obsługa argumentów wiersza poleceń jest standardowym sposobem komunikacji z użytkownikiem w różnych aplikacjach, dlatego warto poznać tę funkcjonalność, aby móc pisać bardziej profesjonalne i intuicyjne programy.

## Zobacz także

- Dokumentacja Java o obsłudze argumentów wiersza poleceń: https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html
- Przykładowe projekty GitHub, wykorzystujące argumenty wiersza poleceń w Java: https://github.com/search?q=command+line+arguments+java