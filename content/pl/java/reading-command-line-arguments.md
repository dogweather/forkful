---
title:                "Czytanie argumentów linii poleceń"
html_title:           "Bash: Czytanie argumentów linii poleceń"
simple_title:         "Czytanie argumentów linii poleceń"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Czytanie argumentów linii komend w Java

## Co i Dlaczego?

Czytanie argumentów linii komend to proces ekstrakcji danych wejściowych wprowadzanych przez użytkownika podczas uruchamiania programu. Programiści robią to, aby umożliwić personalizację funkcji programu.

## Jak Zrobić:

Do ekstrakcji argumentów linii komend używamy tablicy `args` w metodzie `main`.

```Java
public class CommandLine {
    public static void main(String[] args) {
        for (String arg: args) {
            System.out.println("Argument: "+ arg);
        }
    }
}
```
Podając argumenty `Hello World` podczas uruchamiania programu, otrzymać powinniśmy wynik:

```Bash
Argument: Hello
Argument: World
```

## Pogłębione Informacje:

(1) Historia: Czytanie argumentów linii komend jest fundamentem wielu programów, odmianjąc funkcje w zależności od wprowadzonych przez użytkownika danych.

(2) Alternatywy: Biblioteki takie jak JCommander czy Apache Commons CLI oferują bardziej zaawansowane metody obsługi argumentów linii komend.

(3) Szczegóły implementacji: `args` to tablica, z indeksem zaczynającym się od 0. Każdy element tablicy reprezentuje oddzielny argument.

## Zobacz także:

Poradnik Oracle 'Command-Line Arguments' dostarcza wyczerpujących informacji na temat tego, jak Java obsługuje argumenty linii poleceń: https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html

Dokumentacje dla powyższych bibliotek dostępne są tutaj: JCommander (http://jcommander.org/) oraz Apache Commons CLI (https://commons.apache.org/proper/commons-cli/).