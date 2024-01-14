---
title:    "Kotlin: Odczytywanie argumentów wiersza poleceń"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Dlaczego czytać argumenty wiersza poleceń?

Czytanie argumentów wiersza poleceń jest ważną umiejętnością dla każdego programisty. Pozwala ono na dynamiczne przekazywanie informacji do programu podczas jego uruchamiania, co daje możliwość dostosowania działania naszych programów bez konieczności zmian w kodzie. W tym artykule dowiesz się, dlaczego warto nauczyć się czytać argumenty wiersza poleceń.

## Jak czytać argumenty wiersza poleceń?

W języku Kotlin możemy skorzystać z klasy `args`, aby odczytać argumenty podane podczas uruchamiania programu. Sprawdźmy prosty przykład:

```Kotlin
fun main(args: Array<String>) {
    if(args.size > 0) {
        println("Pierwszy argument to: ${args[0]}")
    } else {
        println("Brak podanych argumentów")
    }
}
```

Gdy uruchomimy ten program z argumentem `Hello`, otrzymamy następujący wynik: `Pierwszy argument to: Hello`. Jak widać, argumenty wiersza poleceń przechowywane są w tablicy typu `Array<String>`, a następnie możemy z nich korzystać za pomocą indeksów.

Jeśli chcemy przekazać więcej niż jeden argument, wystarczy je oddzielić spacją podczas uruchomienia programu. Na przykład: `KotlinBlog Post`. Wtedy w tablicy `args` będziemy mieć dostęp do dwóch argumentów: `KotlinBlog` i `Post`.

## Deep Dive

Podczas czytania argumentów wiersza poleceń warto wiedzieć o kilku dodatkowych rzeczach. Po pierwsze, argumenty są czytane jako ciągi znaków, więc musimy dokonać konwersji, jeśli chcemy przetworzyć je jako liczby lub inne typy danych.

Kolejną ważną informacją jest to, że argumenty są czytane w kolejności, w jakiej są podane podczas uruchamiania programu. Jeśli chcemy, aby niektóre argumenty były opcjonalne lub mogły być przekazywane w różnej kolejności, warto skorzystać z biblioteki `ArgParser`, która pozwala na bardziej zaawansowane parsowanie argumentów.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o czytaniu argumentów wiersza poleceń w Kotlinie, polecamy przeczytać następujące artykuły:

- [Oficjalna dokumentacja Klasa opcji wiersza poleceń w Kotlinie](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-array/-option.html)
- [Poradnik na temat czytania warunkowych argumentów w Kotlinie](https://medium.com/chakray/kotlin-conditional-command-line-arguments-372d728df020)