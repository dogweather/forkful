---
title:                "Java: Tworzenie tymczasowego pliku"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego warto tworzyć plik tymczasowy?

Tworzenie plików tymczasowych jest częstą praktyką w programowaniu, której warto poświęcić swoją uwagę. Pliki tymczasowe są używane do przechowywania danych tymczasowych lub do wykonania pewnych zadań w trakcie działania programu. Mogą być także pomocne w utrzymywaniu porządku i czystości w kodzie źródłowym.

## Jak to zrobić?

Tworzenie plików tymczasowych jest możliwe za pomocą kilku prostych kroków w języku Java. Wystarczy użyć odpowiedniej klasy i metod, aby stworzyć plik tymczasowy o określonym rozszerzeniu czy nazwie.

```Java
import java.io.*;

public class TempFileExample {

    public static void main(String[] args) {
        
        try {
            // utworzenie pliku tymczasowego z rozszerzeniem ".tmp"
            File tempFile = File.createTempFile("temp", ".tmp");

            // odwołanie do pliku tymczasowego za pomocą obiektu File
            System.out.println("Nazwa pliku tymczasowego: " + tempFile.getName());

            // ustalenie, że plik tymczasowy zostanie automatycznie usunięty po zakończeniu programu
            tempFile.deleteOnExit();

        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```
Po uruchomieniu powyższego kodu, powinniśmy otrzymać następujący wynik:

```
Nazwa pliku tymczasowego: temp3545583383336279967.tmp
```

Widzimy więc, że plik tymczasowy został utworzony zgodnie z naszymi oczekiwaniami.

## W głębsze wody

Istnieje kilka rodzajów plików tymczasowych, które różnią się sposobem utworzenia i przechowywania danych. Jednym z najczęściej używanych jest plik tymczasowy bez nazwy, który jest tworzony przez system operacyjny i automatycznie usunięty po zakończeniu działania programu. Innymi rodzajami są pliki tymczasowe przechowujące dane w formie wykonywalnej lub obiektów serializowalnych. Warto zwrócić uwagę na ustawianie uprawnień do plików tymczasowych, aby uniknąć potencjalnych problemów z dostępem lub bezpieczeństwem.

## Zobacz także

- [Java File API](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Jak utworzyć i zarządzać plikami tymczasowymi w Javie](https://www.baeldung.com/java-temporary-files)
- [Porady i triki w programowaniu Java](https://javastart.pl/kurs/java/konsola/)