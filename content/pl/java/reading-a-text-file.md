---
title:                "Java: Czytanie pliku tekstowego"
simple_title:         "Czytanie pliku tekstowego"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy chcesz nauczyć się czytać pliki tekstowe w języku Java? W artykule tym dowiesz się, jak to zrobić, aby zacząć przetwarzać pliki tekstowe za pomocą swojego kodu.

## Jak to zrobić

Aby odczytać plik tekstowy w Java, musimy użyć klasy FileReader oraz BufferedReader. W prosty sposób możemy to zrobić za pomocą poniższego kodu:

```Java

// importujemy niezbędne klasy
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class ReadFile {

    public static void main(String[] args) {

        // określamy ścieżkę do pliku tekstowego
        String filePath ="tekst.txt";

        // otwieramy plik i tworzymy BufferedReader
        try (BufferedReader br = new BufferedReader(new FileReader(filePath))) {

            // zmienna przechowująca kolejną linię tekstu
            String line;

            // odczytujemy kolejne linie dopóki nie natrafimy na null
            while ((line = br.readLine()) != null) {
                System.out.println(line);
            }

        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Po uruchomieniu tego kodu, otrzymamy wydruk tekstu z naszego pliku. Na przykład:

```
To jest pierwsza linia tekstu.
To jest druga linia tekstu.
To jest trzecia linia tekstu.
```

## Deep Dive

Podczas wczytywania pliku tekstowego, konieczne jest również obsłużenie wyjątków takich jak FileNotFoundException oraz IOException. Możemy również zmienić sposób wczytywania tekstu, na przykład używając metody read() z klasy FileReader lub wykorzystując pętlę z warunkiem while z użyciem metody readLine().

## Zobacz również

- [Tutorial: Reading and Writing Files in Java](https://www.baeldung.com/java-read-write-files)
- [Java File Handling](https://www.geeksforgeeks.org/file-handling-java-tips/)
- [Official Java Documentation: Reading and Writing Files](https://docs.oracle.com/javase/tutorial/essential/io/file.html)