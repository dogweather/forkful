---
title:                "Kotlin: Sprawdzanie czy istnieje katalog"
simple_title:         "Sprawdzanie czy istnieje katalog"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Dlaczego sprawdzić, czy istnieje katalog?

Czasami w trakcie pisania programów potrzebujemy upewnić się, czy dany katalog istnieje, przed wykonaniem na nim jakichkolwiek operacji. Sprawdzenie istnienia katalogu jest ważne, ponieważ jeśli katalog nie istnieje, może to doprowadzić do błędów w naszym kodzie. W tym artykule dowiesz się, jak w prosty sposób sprawdzić, czy dany katalog istnieje w języku Kotlin.

## Jak to zrobić?

Sprawdzenie, czy dany katalog istnieje, jest możliwe dzięki użyciu funkcji `exists()` na obiekcie typu `File`, która zwraca wartość logiczną `true` lub `false`. Oto przykładowy kod w języku Kotlin:

```Kotlin
val directory = File("sciezka/do/katalogu")
if (directory.exists()) {
    println("Katalog istnieje.")
} else {
    println("Katalog nie istnieje.")
}
```

Jeśli katalog istnieje, na ekranie zostanie wyświetlony komunikat "Katalog istnieje." W przeciwnym wypadku zostanie wyświetlony komunikat "Katalog nie istnieje." Proste, prawda?

Jeśli chcesz sprawdzić istnienie katalogu w konkretnej lokalizacji, poza podaniem pełnej ścieżki dostępu, możesz również wykorzystać funkcję `exists()` w połączeniu z funkcją `isDirectory()`:

```Kotlin
val directory = File("sciezka/do/katalogu")
if (directory.exists() && directory.isDirectory) {
    println("Katalog istnieje w podanej lokalizacji.")
} else {
    println("Katalog nie istnieje w podanej lokalizacji.")
}
```

Funkcja `isDirectory()` sprawdza, czy dany obiekt typu `File` jest katalogiem. Dzięki temu możemy mieć pewność, że funkcja `exists()` zwróci wartość logiczną `true` tylko wtedy, gdy dany katalog rzeczywiście istnieje w podanej lokalizacji.

## Głębsze zanurzenie

Sprawdzenie istnienia katalogu może być bardziej skomplikowane, gdy chcemy go przeszukiwać w poszukiwaniu konkretnego pliku lub katalogu wewnątrz niego. W takiej sytuacji pomocne będą nam funkcje `listFiles()` oraz `listFilesRecursively()`:

```Kotlin
val directory = File("sciezka/do/katalogu")
if (directory.exists()) {
    println("Katalog istnieje.")
    // Przeszukanie katalogu w poszukiwaniu pliku lub katalogu:
    val file = directory.listFiles()?.firstOrNull { it.name == "nazwa_pliku" || it.name == "nazwa_katalogu" }
    if (file != null) {
        println("Znaleziono plik lub katalog.")
    } else {
        println("Nie udało się znaleźć pliku lub katalogu.")
    }
    // Przeszukanie katalogu w poszukiwaniu pliku lub katalogu rekursywnie:
    val fileRecursively = directory.listFilesRecursively()?.firstOrNull { it.name == "nazwa_pliku" || it.name == "nazwa_katalogu" }
    if (fileRecursively != null) {
        println("Znaleziono plik lub katalog rekursywnie.")
    } else {
        println("Nie udało się znaleźć pliku lub katalogu rekursywnie.")
    }
} else {
    println("Katalog nie istnieje.")
}
```

Pierwsza funkcja `listFiles()` zwraca tablicę plików znajdujących się w danym katalogu, natomiast druga funkcja `listFilesRecursively()` przeszukuje katalog rekursywnie, czyli wraz ze wszystkimi podkatalogami.

## Zobacz również

- [Kotlin - Java File class](https://www.tutorialkart.com/kotlin/file-class/)
- [Documentation - File (Kotlin