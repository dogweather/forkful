---
title:                "Odczytywanie argumentów w wierszu poleceń"
html_title:           "Java: Odczytywanie argumentów w wierszu poleceń"
simple_title:         "Odczytywanie argumentów w wierszu poleceń"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Czy wiesz, że w Javie istnieje możliwość czytania argumentów z wiersza poleceń? Programiści często wykorzystują tę funkcję, aby otrzymywać dane od użytkownika lub dostosowywać zachowanie swojego programu w zależności od podanych argumentów.

## Jak to zrobić:
Aby odczytać argumenty z wiersza poleceń, wystarczy skorzystać z metody `main` w naszej klasie i przekazać jako jej parametr tablicę typu `String[]` o nazwie `args`. Następnie wewnątrz metody możemy wykorzystać pętle `for` lub `while` aby przeiterować przez elementy tej tablicy i wyświetlić je lub wykonać na nich jakieś działania. Przykładowy kod wyglądałby następująco:

```Java
public static void main(String[] args) {
    for (String arg : args) {
        System.out.println("Otrzymany argument: " + arg);
    }
}
```

Po uruchomieniu programu z argumentami np. `java MojaKlasa argument1 argument2` otrzymalibyśmy na konsoli następujący wynik:

```
Otrzymany argument: argument1
Otrzymany argument: argument2
```

## Głębszy wgląd:
Funkcjonalność odczytywania argumentów z wiersza poleceń została wprowadzona wraz z wydaniem Javy 1.1 w 1996 roku. Wcześniej programiści musieli polegać na ustawianiu zmiennych przez interfejs graficzny lub interakcję z użytkownikiem w trakcie działania programu. Obecnie w Javie istnieje także możliwość korzystania z zewnętrznych bibliotek, które ułatwiają czytanie argumentów z wiersza poleceń lub oferują dodatkowe funkcje, takie jak parsowanie argumentów.

## Zobacz też:
* Dokumentacja Javy na temat odczytywania argumentów z wiersza poleceń: https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html
* Wprowadzenie do języka Java: https://javastart.pl/static/categories/wprowadzenie-do-java
* Forum dyskusyjne na temat programowania w Javie: https://forum.java.pl/