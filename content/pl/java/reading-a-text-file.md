---
title:                "Czytanie pliku tekstowego"
html_title:           "C: Czytanie pliku tekstowego"
simple_title:         "Czytanie pliku tekstowego"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Czytanie pliku tekstowego to proces, w którym program odczytuje dane zapisane w pliku tekstowym. Programiści robią to, aby manipulować danymi zapisanymi na dysku, a nie tylko w pamięci.

## Jak to zrobić:

Oto przykładowy kod pokazujący, jak czytać plik tekstowy w Javie.

```Java
import java.nio.file.*;

public class ReadFile {
    public static void main(String[] args) throws Exception {
        Path path = Paths.get("example.txt");
        byte[] bytes = Files.readAllBytes(path);
        String content = new String(bytes);

        System.out.println(content);
    }
}
```

Uruchomienie tego kodu da wyjście równoznaczne z zawartością pliku "example.txt".

## Deep Dive

**Historycznie** czytanie pliku tekstowego sięga początków programowania. Java, od wersji 7 (2011 r.), udostępnia API *NIO (New Input/Output)* do lepszego zarządzania I/O takich jak operacje na plikach.

**Alternatywy** to korzystanie ze starszych klas, takich jak *FileReader* i *BufferedReader*, które oferują większą kontrolę, ale są bardziej skomplikowane w użyciu.

```Java
import java.io.*;

public class ReadFile {
    public static void main(String[] args) throws Exception {
        File file = new File("example.txt");
        BufferedReader br = new BufferedReader(new FileReader(file));
        
        String line;
        while ((line = br.readLine()) != null) {
            System.out.println(line);
        }
        br.close();
    }
}
```

**Szczegóły implementacji**: metoda `Files.readAllBytes` odczytuje cały plik jako jedną dużą partię danych, co nie jest efektywne dla dużych plików. Klasa `BufferedReader` odczytuje plik linia po linii, co jest dużo bardziej efektywne dla dużych plików.

## Zobacz także

1. [Dokumentacja Java NIO](https://docs.oracle.com/javase/8/docs/api/java/nio/package-summary.html) - szczegółowa dokumentacja o klasach Java NIO.
2. [Przewodnik Oracle's File I/O](https://docs.oracle.com/javase/tutorial/essential/io/) - bardziej szczegółowy przewodnik po I/O w Javie.