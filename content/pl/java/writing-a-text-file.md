---
title:                "Java: Pisanie pliku tekstowego"
simple_title:         "Pisanie pliku tekstowego"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego pisać plik tekstowy?

Pisanie plików tekstowych jest nieodłączną częścią tworzenia oprogramowania przy użyciu języka Java. Pliki te są wykorzystywane do przechowywania i przesyłania danych w formie tekstowej, w celu ułatwienia komunikacji między różnymi aplikacjami. Oprócz tego, pisanie plików tekstowych jest często wykorzystywane do debugowania i analizy danych.

## Jak to zrobić?

Aby utworzyć plik tekstowy przy użyciu języka Java, należy wykonać kilka prostych kroków.

### 1. Importowanie pakietu

Pierwszym krokiem jest zaimportowanie pakietu java.io, który zawiera klasy i metody do obsługi plików.

```java
import java.io.*;
```

### 2. Utworzenie obiektu File

Następnie należy utworzyć obiekt klasy File, który będzie reprezentował nowo utworzony plik. W nawiasach konstruktora podajemy ścieżkę i nazwę pliku.

```java
File file = new File("sciezka/do/pliku/nowy_plik.txt");
```

### 3. Utworzenie obiektu FileWriter

Kolejnym krokiem jest utworzenie obiektu klasy FileWriter, który będzie służył do zapisywania danych do pliku.

```java
FileWriter writer = new FileWriter(file);
```

### 4. Zapisywanie danych

Teraz możemy przystąpić do zapisywania danych do pliku. Do tego celu wykorzystujemy metodę write, podając jako argumenty tekst do zapisania.

```java
writer.write("To jest przykładowy tekst, który zostanie zapisany do pliku.");
```

### 5. Zamknięcie pliku

Na koniec musimy zamknąć plik, wywołując metodę close na obiekcie FileWriter.

```java
writer.close();
```

Cały kod wyglądałby w ten sposób:

```java
import java.io.*;

public class Main {
    public static void main(String[] args) throws IOException {
        File file = new File("sciezka/do/pliku/nowy_plik.txt");
        FileWriter writer = new FileWriter(file);
        writer.write("To jest przykładowy tekst, który zostanie zapisany do pliku.");
        writer.close();
    }
}
```

## Głębszy zanurzenie

Pliki tekstowe można także odczytywać i przetwarzać z wykorzystaniem innych klas, takich jak FileInputStream, BufferedReader czy Scanner. Warto także zapoznać się z klasą PrintWriter, która umożliwia łatwe formatowanie tekstu i zapisywanie go do plików.

Ponadto, istnieje wiele zaawansowanych technik manipulacji plikami tekstowymi, takich jak parsowanie XML czy wykorzystywanie bibliotek do obsługi formatów CSV. Więcej informacji na ten temat można znaleźć w dokumentacji Java lub na różnych forach i blogach poświęconych programowaniu.

## Zobacz także

- [Writing Files in Java](https://www.baeldung.com/java-write-to-file)
- [Java File Handling](https://www.geeksforgeeks.org/file-handling-java/)
- [Commons IO Library](https://commons.apache.org/proper/commons-io/)