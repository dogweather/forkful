---
title:                "Tworzenie pliku tymczasowego"
html_title:           "Java: Tworzenie pliku tymczasowego"
simple_title:         "Tworzenie pliku tymczasowego"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Dlaczego

Tworzenie plików tymczasowych jest przydatnym narzędziem dla programistów Java, gdyż pozwala ono na przechowywanie danych tymczasowych i tym samym ułatwia proces tworzenia oprogramowania.

# Jak to zrobić

Aby utworzyć plik tymczasowy w Java, możemy skorzystać z klasy `File` i jej metody `createTempFile()`. Przykładowy kod wyglądałby następująco:

```Java
// Importowanie klasy File
import java.io.File;

// Tworzenie pliku tymczasowego o nazwie temp i rozszerzeniu txt
File temp = File.createTempFile("temp", ".txt");

// Wypisanie ścieżki do utworzonego pliku
System.out.println("Ścieżka pliku tymczasowego: " + temp.getAbsolutePath());
```

Po uruchomieniu powyższego kodu, w katalogu tymczasowym zostanie utworzony plik o nazwie "temp" i rozszerzeniu ".txt". Ponadto, jego ścieżka zostanie wypisana na konsolę.

# Głębszy wgląd

Podczas tworzenia pliku tymczasowego, możemy określić również ścieżkę, w której ma zostać on utworzony. W tym celu, należy skorzystać z przeciążonej metody `createTempFile()` z dwoma parametrami: prefiksem nazwy pliku oraz miejscem, gdzie ma zostać utworzony. Przykładowy kod wyglądałby następująco:

```Java
// Tworzenie pliku tymczasowego z prefiksem "myTemp" w podanej ścieżce
File temp = File.createTempFile("myTemp", null, new File("C:/myFolder"));
```

Ponadto, przy tworzeniu pliku tymczasowego możemy określić również prefiks oraz sufiks nazwy pliku, a także katalog, w którym ma zostać utworzony.

Podsumowując, tworzenie plików tymczasowych jest przydatnym i prostym narzędziem w świecie programowania Java. Dzięki temu, możemy wygodnie przechowywać dane tymczasowe, co ułatwia proces tworzenia oprogramowania.

# Zobacz również

- [Dokumentacja klasy File w języku Java](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/File.html)
- [Poradnik tworzenia plików tymczasowych w Java](https://www.baeldung.com/java-temporary-file)