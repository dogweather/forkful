---
title:                "Odczytywanie pliku tekstowego"
aliases:
- /pl/java/reading-a-text-file.md
date:                  2024-01-20T17:54:29.807549-07:00
model:                 gpt-4-1106-preview
simple_title:         "Odczytywanie pliku tekstowego"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Czytanie pliku tekstowego to proces pozyskiwania danych z zapisanego tekstu. Programiści robią to, by obsłużyć informacje – konfigurować aplikacje, analizować dane lub po prostu wczytywać instrukcje.

## Jak to zrobić:
Do wczytywania plików tekstowych w Javie wykorzystujemy klasę `Files` z pakietu `java.nio.file`, która oferuje metodę `readAllLines`. Poniżej prosty przykład:

```java
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

public class ReadTextFileExample {
    public static void main(String[] args) {
        try {
            Path filePath = Path.of("example.txt");
            List<String> lines = Files.readAllLines(filePath);
            lines.forEach(System.out::println);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Przy założeniu, że w pliku `example.txt` znajduje się:

```
Witaj w pliku tekstowym.
To jest druga linia.
```

Wynik wykonania programu:

```
Witaj w pliku tekstowym.
To jest druga linia.
```

## Pogłębiona wiedza
Czytanie plików tekstowych ewoluowało w Javie. Kiedyś dominowała klasa `FileReader` w połączeniu z `BufferedReader`. Dziś preferujemy `Files` z `java.nio.file` dla lepszej wydajności i prostoty.

Alternatywy to na przykład `Scanner` do czytania danych z różnych źródeł włączając pliki, czy `FileInputStream` dla binarnych danych. 

Ważne: Obsługa wyjątków i kodowania to kluczowe aspekty. `readAllLines` domyślnie używa UTF-8, co jest bezpieczne dla większości języków, wliczając polski.

## Zobacz również
- Dokumentacja `Files.readAllLines`: https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/nio/file/Files.html#readAllLines(java.nio.file.Path)
- Tutorial Oracle do NIO.2 (nowe wejście/wyjście): https://docs.oracle.com/javase/tutorial/essential/io/fileio.html
- Ewolucja wejścia/wyjścia w Javie (ang.): https://www.baeldung.com/java-io-vs-nio
