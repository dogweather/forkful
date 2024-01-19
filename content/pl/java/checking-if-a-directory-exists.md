---
title:                "Sprawdzanie, czy katalog istnieje"
html_title:           "Java: Sprawdzanie, czy katalog istnieje"
simple_title:         "Sprawdzanie, czy katalog istnieje"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Sprawdzanie czy katalog istnieje w Java

## Co i dlaczego?

Sprawdzanie, czy katalog istnieje, polega na weryfikacji, czy dany katalog istnieje w systemie plików. Programiści robią to, aby zapobiec błędom podczas pracy z plikami.

## Jak to zrobić:

Java udostępnia klasę `Files` do manipulacji plikami i katalogami. Możemy użyć metody `exists(Path)` do sprawdzenia, czy dany katalog istnieje. Oto jak to zrobić:

```Java
import java.nio.file.*;

public class Main {
    public static void main(String[] args) {
        Path path = Paths.get("/sciezka/do/katalogu");
        
        if (Files.exists(path)) {
            System.out.println("Katalog istnieje");
        } else {
            System.out.println("Katalog nie istnieje");
        }
    }
}
```

Wynik działania programu:

```Java
Katalog istnieje
lub
Katalog nie istnieje
```

## Pogłębione informacje

- Historyczny kontekst: Wcześniejsze wersje Javy wymagały korzystania z klasy `File` do manipulacji plikami i katalogami. Nowoczesne wersje Javy (od verzji 7), wprowadziły nową bibliotekę `java.nio.file` z klasą `Path` i `Files`, która oferuje bardziej intuicyjne i elastyczne API.
  
- Alternatywy: Możemy również użyć klasy `File` w Javie do sprawdzenia, czy katalog istnieje. Metoda `File.exists()` zwraca `true`, jeśli katalog istnieje, i `false` w przeciwnym razie.
  
- Szczegóły implementacji: Metoda `java.nio.file.Files.exists(Path)` używa natywnej implementacji systemu plików do sprawdzenia, czy dany plik lub katalog istnieje. Działa to zarówno dla lokalnych systemów plików, jak i sieciowych systemów plików, takich jak NFS.

## Zobacz także

- [Dokumentacja Oracle na temat klasy Files](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/nio/file/Files.html)
  
- [Dokumentacja Oracle na temat klasy Path](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/nio/file/Path.html)
  
- [Dokumentacja Oracle na temat klasy File](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)