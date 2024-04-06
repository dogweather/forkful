---
date: 2024-01-20 17:40:32.604185-07:00
description: "How to: Pliki tymczasowe istniej\u0105 od czas\xF3w wcze\u015Bniejszych\
  \ system\xF3w operacyjnych. W Javie, od JDK 1.2, mo\u017Cemy u\u017Cywa\u0107 `File.createTempFile`,\
  \ a od Javy 7\u2026"
lastmod: '2024-04-05T21:53:36.735694-06:00'
model: gpt-4-1106-preview
summary: "Pliki tymczasowe istniej\u0105 od czas\xF3w wcze\u015Bniejszych system\xF3\
  w operacyjnych."
title: Tworzenie pliku tymczasowego
weight: 21
---

## How to:
```java
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class TemporaryFileExample {

    public static void main(String[] args) {
        try {
            // Tworzymy plik tymczasowy
            File tempFile = Files.createTempFile("myApp", ".txt").toFile();
            System.out.println("Plik tymczasowy utworzony: " + tempFile.getAbsolutePath());
            
            // Zapisujemy dane do pliku (opcjonalne)
            Files.writeString(tempFile.toPath(), "Przykładowy tekst");

            // Odczytujemy dane z pliku (opcjonalne)
            String content = Files.readString(tempFile.toPath());
            System.out.println("Zawartość pliku: " + content);

            // Usuwamy plik tymczasowy (powinno być wykonane przed zakończeniem programu)
            tempFile.deleteOnExit();
            System.out.println("Plik tymczasowy będzie usunięty po wyjściu z programu.");
        } catch (IOException e) {
            System.err.println("Wystąpił błąd przy tworzeniu pliku tymczasowego: " + e.getMessage());
        }
    }
}
```
Wynik:
```
Plik tymczasowy utworzony: ścieżka_do_pliku
Zawartość pliku: Przykładowy tekst
Plik tymczasowy będzie usunięty po wyjściu z programu.
```

## Deep Dive
Pliki tymczasowe istnieją od czasów wcześniejszych systemów operacyjnych. W Javie, od JDK 1.2, możemy używać `File.createTempFile`, a od Javy 7 `Files.createTempFile` z NIO.2, które oferuje lepszą kontrolę nad atrybutami pliku. Alternatywnie możemy uzyskać katalog tymczasowy przez `System.getProperty("java.io.tmpdir")` i tworzyć w nim pliki ręcznie.

Bezpieczeństwo plików tymczasowych jest istotne; warto używać unikalnych nazw (co zapewnia metoda `createTempFile`) i usunąć pliki po użyciu. `deleteOnExit()` jest przydatne, ale uwaga: jeśli stworzysz wiele plików tymczasowych i nie usuniesz ich explicitnie, mogą zająć pamięć do restartu JVM. Ponadto zastosowanie try-catch jest zalecane do detekcji i obsługi wyjątków IO.

## See Also
- [Klasa Files (JavaDoc)](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/nio/file/Files.html)
- [Klasa File (JavaDoc)](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
- [Java NIO File API](https://docs.oracle.com/javase/tutorial/essential/io/fileio.html)
