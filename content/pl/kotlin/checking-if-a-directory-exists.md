---
title:                "Weryfikowanie istnienia katalogu"
html_title:           "Kotlin: Weryfikowanie istnienia katalogu"
simple_title:         "Weryfikowanie istnienia katalogu"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Sprawdzanie, czy dany katalog istnieje, jest ważnym krokiem w wielu aplikacjach. Może to pomóc w uniknięciu błędów lub zapewnieniu, że aplikacja działa w oczekiwany sposób.

## Jak to zrobić?

Sprawdzenie istnienia katalogu w języku Kotlin jest bardzo proste. Wystarczy użyć metody `exists()` z klasy `File`. Poniżej znajduje się przykładowy kod z wykorzystaniem tej metody:

```Kotlin
val directory = File("path/to/directory")

if(directory.exists()){
  println("Katalog istnieje!")
} else {
  println("Katalog nie istnieje!")
}
```

W tym przykładzie, najpierw tworzymy obiekt `File` z podaną ścieżką do katalogu. Następnie w warunku `if` wywołujemy metodę `exists()` na tym obiekcie. Jeśli zwróci ona wartość `true`, oznacza to, że dany katalog istnieje.

Jeśli chcesz sprawdzić istnienie katalogu w konkretnym miejscu w twoim systemie plików, możesz również użyć metody `exists(path: String)` z klasy `File`. Przykład:

```Kotlin
if(File.exists("another/directory/path")){
  println("Katalog istnieje!")
} else {
  println("Katalog nie istnieje!")
}
```

## Deep Dive

Podczas sprawdzania istnienia katalogu, warto zwrócić uwagę na kilka rzeczy. Po pierwsze, należy pamiętać, że metoda `exists()` może również zwrócić wartość `true`, jeśli podana ścieżka wskazuje na plik, a nie na katalog. Dlatego warto użyć dodatkowej metody, np. `isDirectory()`, aby upewnić się, że obiekt `File` odnosi się do katalogu.

Kolejną rzeczą, na którą warto zwrócić uwagę, jest to, że metoda `exists()` może również zwrócić wartość `false`, jeśli nie masz uprawnień do sprawdzanego katalogu. W takim przypadku warto sprawdzić, czy masz odpowiednie uprawnienia lub czy katalog istnieje w ogóle.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o pracy z plikami i katalogami w języku Kotlin, polecamy zapoznać się z poniższymi artykułami:

- [Tworzenie i modyfikowanie plików w języku Kotlin] (https://kotlinlang.org/docs/tutorials/java-interop.html#creating-and-modifying-files)
- [Zarządzanie katalogami] (https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/-file/)
- [Tworzenie i działanie z wyjątkami] (https://kotlinlang.org/docs/reference/exceptions.html)