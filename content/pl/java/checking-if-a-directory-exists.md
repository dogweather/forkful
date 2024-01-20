---
title:                "Sprawdzanie, czy katalog istnieje"
date:                  2024-01-20T14:57:10.650048-07:00
html_title:           "Fish Shell: Sprawdzanie, czy katalog istnieje"
simple_title:         "Sprawdzanie, czy katalog istnieje"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
"Co i dlaczego?"

Sprawdzanie, czy katalog istnieje w Java, to metoda weryfikacji obecności folderu w systemie plików. Robimy to, by uniknąć błędów podczas operacji na plikach i folderach, na przykład przed próbą odczytu zawartości nieistniejącego katalogu.

## How to:
"Jak to zrobić:"

Sprawdzanie istnienia katalogu jest łatwe dzięki klasie `Files` i `Path`:

```java
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class DirectoryExists {
    public static void main(String[] args) {
        Path path = Paths.get("/jakas/sciezka/do/katalogu");

        boolean exists = Files.exists(path);
        boolean notExists = Files.notExists(path);
        
        if (exists) {
            System.out.println("Katalog istnieje.");
        } else if (notExists) {
            System.out.println("Katalog nie istnieje.");
        } else {
            System.out.println("Status katalogu nieznany.");
        }
    }
}
```

Gdy uruchomimy kod, otrzymamy jeden z wyników:

```
Katalog istnieje.
```

Lub:

```
Katalog nie istnieje.
```

## Deep Dive:
"Głębsze spojrzenie:"

Sprawdzanie, czy katalog istnieje, jest częścią zarządzania systemem plików w każdym języku programowania. W Javie, przed wprowadzeniem NIO.2 (New Input/Output 2) w Java SE 7, głównie używano klasy `File`. Po wprowadzeniu `java.nio.file` zyskaliśmy bardziej intuicyjne API do pracy z systemem plików.

Główne różnice:
- `Files.exists(Path)` jest lepsze od `File.exists()`, bo obsługuje lepiej symboliczne linki i jest lepiej zintegrowane z wyjątkami.
- Wątpliwości związane z `Files.exists` i `Files.notExists` można rozstrzygnąć przez `Files.isDirectory(Path, LinkOption...)`, co bezpośrednio sprawdza, czy ścieżka jest katalogiem.

Alternatywnie, można użyć `File.isDirectory()` z klasy `File`, ale jej użycie obecnie jest uważane za mniej pożądane ze względu na lepszą funkcjonalność oferowaną przez `java.nio.file`.

## See Also:
"Zobacz także:"

- Dokumentacja Oracle dla klasy `Files`: https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html
- Porównanie IO i NIO w Javie: https://www.baeldung.com/java-nio-2-file-api
- Przewodnik po Java NIO: https://www.jenkov.com/tutorials/java-nio/index.html