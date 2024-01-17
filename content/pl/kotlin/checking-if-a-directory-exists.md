---
title:                "Sprawdzanie czy katalog istnieje"
html_title:           "Kotlin: Sprawdzanie czy katalog istnieje"
simple_title:         "Sprawdzanie czy katalog istnieje"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Sprawdzanie czy katalog istnieje jest procesem, który polega na wykrywaniu istnienia określonego katalogu w systemie plików. Programiści często wykonują to zadanie, aby upewnić się, czy dany katalog został utworzony lub czy istnieje już w strukturze plików.

## Jak:

```Kotlin
val directory = File("ścieżka/do/katalogu")
val exists = directory.exists()
println(exists) // true jeśli katalog istnieje, false jeśli nie istnieje
```

## Głębsze zanurzenie:

1. Kontekst historyczny: W starszych wersjach języka Kotlin, sprawdzanie istnienia katalogu było wykonane przez funkcję "File.isDirectory()". Jednak wraz z aktualizacją do wersji 1.0, zaleca się używanie funkcji "File.exists()" zamiast stosowania metody.

2. Alternatywy: Istnieją również inne sposoby na sprawdzenie istnienia katalogu w Kotlinie, takie jak wykorzystanie klasy "java.nio.file.Path" lub użycie biblioteki polecenia systemowego "java.nio.file.Files".

3. Szczegóły implementacji: Funkcja "File.exists()" zwraca wartość typu boolean, czyli true jeśli katalog istnieje lub false, jeśli nie istnieje. Wykorzystuje ona metody systemowe do odnalezienia ścieżki do katalogu i sprawdzenia, czy istnieje.

## Zobacz też:

- Oficjalna dokumentacja języka Kotlin: https://kotlinlang.org/
- Przewodniki i poradniki dla programistów w języku Kotlin: https://developer.android.com/kotlin
- Dokumentacja API File w języku Kotlin: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/index.html