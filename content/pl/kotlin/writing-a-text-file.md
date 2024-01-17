---
title:                "Tworzenie pliku tekstowego"
html_title:           "Kotlin: Tworzenie pliku tekstowego"
simple_title:         "Tworzenie pliku tekstowego"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Pisanie plików tekstowych jest ważnym elementem programowania, ponieważ pozwala nam na zapisywanie danych w postaci zrozumiałej dla użytkownika. Programiści często korzystają z tej metody, aby zapisywać informacje, takie jak ustawienia aplikacji lub wyniki obliczeń.

## Jak to zrobić:

```Kotlin
val fileName = "moj_plik.txt"
val file = File(fileName)

// Tworzenie lub nadpisywanie istniejącego pliku
file.printWriter().use { out ->
    out.println("Witaj, to jest mój pierwszy plik!")
    out.println("Możesz tutaj zapisać dowolną treść.")
}

// Dodawanie nowych danych do istniejącego pliku
file.printWriter().appendln().use { out ->
    out.println("To kolejna linijka tekstu.")
}

```

Output w pliku `moj_plik.txt`:

```
Witaj, to jest mój pierwszy plik!
Możesz tutaj zapisać dowolną treść.
To kolejna linijka tekstu.
```

## Wgląd w głąb:

Pisanie plików tekstowych jest popularną metodą zapisywania danych w programowaniu od bardzo dawna. Alternatywą dla tej metody jest wykorzystanie bazy danych, jednak pisanie plików tekstowych jest przydatne w przypadku danych, które nie wymagają złożonej struktury lub przeprowadzania zapytań.

W Kotlinie do pisania plików tekstowych możemy wykorzystać różne metody, takie jak `printWriter()` lub `printStream()`. W obu przypadkach należy pamiętać o ustawieniu trybu "append", jeśli chcemy dodać nowe dane do istniejącego pliku.

## Zobacz także:

- Dokumentacja Kotlina na temat operacji na plikach: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/
- Przykładowe wykorzystanie w programie konsolowym: https://www.geeksforgeeks.org/kotlin-file-handling/
- Przydatny tutorial na temat pisania plików tekstowych w Kotlinie: https://www.tutorialspoint.com/kotlin/kotlin_file_handling.htm