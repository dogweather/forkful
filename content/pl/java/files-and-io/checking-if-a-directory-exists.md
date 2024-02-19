---
aliases:
- /pl/java/checking-if-a-directory-exists/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:11.212983-07:00
description: "Sprawdzanie, czy katalog istnieje w Javie, to podstawowe zadanie polegaj\u0105\
  ce na weryfikacji obecno\u015Bci katalogu w systemie plik\xF3w przed odczytem z\
  \ niego,\u2026"
lastmod: 2024-02-18 23:08:49.492215
model: gpt-4-0125-preview
summary: "Sprawdzanie, czy katalog istnieje w Javie, to podstawowe zadanie polegaj\u0105\
  ce na weryfikacji obecno\u015Bci katalogu w systemie plik\xF3w przed odczytem z\
  \ niego,\u2026"
title: Sprawdzanie, czy katalog istnieje
---

{{< edit_this_page >}}

## Co i dlaczego?
Sprawdzanie, czy katalog istnieje w Javie, to podstawowe zadanie polegające na weryfikacji obecności katalogu w systemie plików przed odczytem z niego, zapisem do niego lub wykonaniem jakichkolwiek operacji wymagających jego istnienia. Jest to kluczowe, aby uniknąć błędów lub wyjątków w programach interaktywnie działających z systemem plików, zapewniając płynniejsze wykonanie i lepsze doświadczenia użytkownika.

## Jak to zrobić:
W Javie istnieje kilka sposobów na sprawdzenie, czy katalog istnieje, głównie za pomocą klas `java.nio.file.Files` i `java.io.File`.

**Korzystając z `java.nio.file.Files`**:

Jest to zalecane podejście w nowszych wersjach Javy.

```java
import java.nio.file.Files;
import java.nio.file.Paths;

public class DirectoryExists {
    public static void main(String[] args) {
        // Tutaj podaj ścieżkę do katalogu
        String directoryPath = "ścieżka/do/katalogu";

        // Sprawdzanie, czy katalog istnieje
        if (Files.exists(Paths.get(directoryPath))) {
            System.out.println("Katalog istnieje.");
        } else {
            System.out.println("Katalog nie istnieje.");
        }
    }
}
```
**Przykładowy wynik**:
```
Katalog istnieje.
```
Albo
```
Katalog nie istnieje.
```

**Korzystając z `java.io.File`**:

Chociaż zalecane jest używanie `java.nio.file.Files`, starsza klasa `java.io.File` może być także używana.

```java
import java.io.File;

public class DirectoryExistsLegacy {
    public static void main(String[] args) {
        // Tutaj podaj ścieżkę do katalogu
        String directoryPath = "ścieżka/do/katalogu";

        // Tworzenie obiektu File
        File directory = new File(directoryPath);

        // Sprawdzanie, czy katalog istnieje
        if (directory.exists() && directory.isDirectory()) {
            System.out.println("Katalog istnieje.");
        } else {
            System.out.println("Katalog nie istnieje.");
        }
    }
}
```
**Przykładowy wynik**:
```
Katalog istnieje.
```
Albo
```
Katalog nie istnieje.
```

**Korzystając z bibliotek stron trzecich**:

Chociaż standardowa biblioteka Java zazwyczaj wystarcza do tego zadania, biblioteki stron trzecich, takie jak Apache Commons IO, oferują dodatkowe narzędzia do obsługi plików, które mogą być użyteczne w bardziej złożonych aplikacjach.

**Apache Commons IO**:

Najpierw dodaj zależność Apache Commons IO do swojego projektu. Następnie możesz użyć jej funkcji do sprawdzenia, czy katalog istnieje.

```java
// Zakładając, że Apache Commons IO zostało dodane do projektu

import org.apache.commons.io.FileUtils;

public class DirectoryExistsCommons {
    public static void main(String[] args) {
        // Tutaj podaj ścieżkę do katalogu
        String directoryPath = "ścieżka/do/katalogu";

        // Korzystanie z FileUtils do sprawdzenia
        boolean directoryExists = FileUtils.directoryContains(new File(directoryPath), null);

        if (directoryExists) {
            System.out.println("Katalog istnieje.");
        } else {
            System.out.println("Katalog nie istnieje.");
        }
    }
}
```

**Uwaga**: `FileUtils.directoryContains` sprawdza, czy w katalogu znajduje się określony plik, ale przekazując `null` jako drugi argument, można go użyć do sprawdzenia istnienia katalogu. Należy być ostrożnym, ponieważ może to nie być najbardziej bezpośrednie lub zamierzone użycie metody.

**Przykładowy wynik**:
```
Katalog istnieje.
```
Albo
```
Katalog nie istnieje.
```
