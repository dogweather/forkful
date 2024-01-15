---
title:                "Odczytywanie argumentów wiersza poleceń"
html_title:           "Kotlin: Odczytywanie argumentów wiersza poleceń"
simple_title:         "Odczytywanie argumentów wiersza poleceń"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego 

Jeśli jesteś programistą lub uczysz się programowania, prawdopodobnie słyszałeś już o języku Kotlin. Jest to język programowania, który zyskuje popularność ze względu na swoją prostotę i wydajność. W tym artykule dowiesz się, dlaczego jest ważne nauczyć się czytać argumenty z wiersza poleceń w języku Kotlin oraz jak to zrobić.

## Jak to zrobić

Czytanie argumentów z wiersza poleceń jest ważną umiejętnością dla każdego programisty. Pozwala to na przekazywanie zewnętrznych danych do naszych programów. W języku Kotlin, aby odczytać argumenty z wiersza poleceń, należy użyć metody ```main```. Poniżej znajduje się kod przykładowy oraz jego wyjście:

```Kotlin
fun main(args: Array<String>) {
    println("Podane argumenty: ")
    for (arg in args) {
        println(arg)
    }
}
```

**Wyjście:**

Podane argumenty:
argument1
argument2
argument3 

Wyjaśnienie kodu: najpierw tworzymy funkcję ```main``` z jednym parametrem ```args```, który jest tablicą ciągów znaków. Następnie wypisujemy informację, że wyświetlamy podane argumenty. W pętli ```for``` iterujemy przez wszystkie argumenty i wypisujemy je na ekranie.

## Deep Dive 

W powyższym przykładzie użyliśmy metody ```Array<String>```, aby odczytać argumenty z wiersza poleceń. Jednak w Kotlinie istnieje również specjalna klasa ```args```, która jest zdefiniowana w pakiecie ```java.lang```. Dzięki temu nie musimy importować pakietu, aby jej używać. Możemy również przypisać argumenty z wiersza poleceń jako listę typu ```ArrayList```. Poniżej znajduje się kod pokazujący wykorzystanie tej klasy:

```Kotlin
fun main(args: Array<String>) {
    val listOfArgs = ArrayList(args.asList())
    println("Pierwszy argument: ${listOfArgs[0]}")
}
```

**Wyjście:**

Pierwszy argument: argument1

Wyjaśnienie kodu: używamy metody ```asList()```, aby przekonwertować argumenty z wiersza poleceń na typ listy ```ArrayList```. Następnie wyświetlamy pierwszy argument z tej listy.

## Zobacz również

1. Dokumentacja języka Kotlin na temat argumentów z wiersza poleceń - https://kotlinlang.org/docs/tutorials/command-line.html
2. Temat odczytywania wejść z wiersza poleceń w języku Java - https://www.javatpoint.com/java-command-line-arguments
3. Poradnik dla początkujących na temat języka Kotlin - https://kotlinlang.org/docs/tutorials/getting-started.html