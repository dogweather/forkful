---
title:    "Kotlin: Odczytywanie argumentów wiersza poleceń"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Dlaczego?

Najprawdopodobniej, jeśli czytasz ten artykuł, to jesteś programistą Kotlin albo jesteś w trakcie nauki tego języka. Jeśli tak, to świetnie! Dzięki temu poradnikowi dowiesz się, jak czytać argumenty wiersza poleceń, co jest istotną umiejętnością w wielu aplikacjach. Nie traćmy więc czasu i przejdźmy do konkretów!

## Jak to zrobić?

### Definiowanie argumentów

Czytanie argumentów wiersza poleceń jest bardzo proste w języku Kotlin. Pierwszym krokiem jest zdefiniowanie listy argumentów jako tablicy napisów (String). Możemy to zrobić używając funkcji `args` z obiektu `System`.

```Kotlin
fun main(args: Array<String>) {
    // kod aplikacji
}
```

### Dostęp do argumentów

Po zdefiniowaniu listy, możemy już przejść do ich użycia w naszej aplikacji. Aby dostać się do argumentów, możemy użyć standardowego iteratora `foreach` lub indeksów tablicy.

```Kotlin
fun main(args: Array<String>) {
    for (arg in args) {
        println("Argument: $arg")
    }
    
    println("Pierwszy argument: ${args[0]}")
}
```

Jeśli chcemy przetestować naszą aplikację, możemy skompilować ją używając komendy `kotlinc` i uruchomić z odpowiednimi argumentami.

`kotlinc NaszaAplikacja.kt -include-runtime -d NaszaAplikacja.jar`
`java -jar NaszaAplikacja.jar argument1 argument2`

Po uruchomieniu z odpowiednimi argumentami, powinniśmy zobaczyć ich listę oraz wartość pierwszego argumentu w konsoli.

### Obsługa błędów

W niektórych przypadkach, użytkownicy mogą zapomnieć wpisać odpowiednie argumenty lub wpisać je w złym formacie. Aby uniknąć wystąpienia błędów w naszej aplikacji, możemy dodać prostą walidację argumentów.

```Kotlin
if (args.size < 2) {
    println("Podaj co najmniej 2 argumenty!")
    return
}
```

### Przykładowe wykorzystanie

Aby lepiej zrozumieć, jak czytać argumenty wiersza poleceń, spójrzmy na prosty przykład. Załóżmy, że mamy prostą aplikację, która wykonuje działania matematyczne z dwoma argumentami podanymi przez użytkownika.

```Kotlin
fun main(args: Array<String>) {
    if (args.size < 2) {
        println("Podaj co najmniej 2 argumenty!")
        return
    }
    
    val a = args[0].toDouble()
    val b = args[1].toDouble()
    
    println("Suma: ${a + b}")
    println("Różnica: ${a - b}")
    println("Iloczyn: ${a * b}")
    println("Iloraz: ${a / b}")
}
```

Jeśli uruchomimy naszą aplikację z argumentami `5 10`, powinna ona zwrócić wyniki kolejnych działań matematycznych z tymi argumentami.

## Warto wiedzieć!

Istnieje także możliwość przekazania argumentów wiersza poleceń w inny sposób niż podawanie ich po uruchomieniu aplikacji. Możemy użyć opcji `Program arguments` w ustawieniach naszego projektu w IntelliJ lub dodawać je w konfiguracji wywołania naszej aplikacji w terminalu.

## Zobacz także

- Dokumentacja Javy o czytaniu argumentów wiersza poleceń: https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html
- Przykładowy projekt na GitHub, który wykorzystuje argumenty wiersza poleceń w Kotlinie: https://github.com/Kotlin/CommandLineProcessor