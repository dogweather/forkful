---
title:                "Odczytywanie pliku tekstowego"
html_title:           "Java: Odczytywanie pliku tekstowego"
simple_title:         "Odczytywanie pliku tekstowego"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Czy kiedykolwiek chciałeś/ chciałaś odczytać zawartość pliku tekstowego za pomocą swojego programu? To właśnie jest czytanie pliku tekstowego - proces, w którym program odczytuje informacje z pliku i wykorzystuje je w swoim działaniu. Programiści często wykonują tę czynność, aby uzyskać dostęp do danych lub konfiguracji, które są przechowywane w pliku.

## Jak to zrobić:
```Java
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class ReadTextFile {

    public static void main(String[] args) {
        try {
            File file = new File("nazwapliku.txt"); //utwórz obiekt reprezentujący plik
            Scanner scanner = new Scanner(file); //utwórz obiekt Scanner dla pliku
            while (scanner.hasNextLine()) { //dopóki plik ma kolejną linię
                String data = scanner.nextLine(); //odczytaj linię i przypisz do zmiennej data
                System.out.println(data); //wyświetl linię
            }
            scanner.close(); //zamknij Scanner
        } catch (FileNotFoundException e) {
            e.printStackTrace(); //obsłuż wyjątek, jeśli plik nie został znaleziony
        }
    }
}
```

Przykładowy plik tekstowy "nazwapliku.txt" może wyglądać następująco:
```
To jest pierwsza linia tekstu.
A to jest druga linia.
Oto trzecia linia.
```

Po uruchomieniu powyższego kodu, otrzymamy następujący wynik:
```
To jest pierwsza linia tekstu.
A to jest druga linia.
Oto trzecia linia.
```

## Deep Dive:
Czytanie pliku tekstowego jest często stosowane w programowaniu od jego początków, kiedy komputery zaczęły obsługiwać dane przechowywane na dyskach twardych. Alternatywą dla czytania plików tekstowych jest korzystanie z bazy danych lub plików typu CSV, które są bardziej zoptymalizowane pod kątem przetwarzania dużych ilości danych. Implementacja czytania pliku tekstowego może być różna w zależności od języka programowania, jednak ogólna koncepcja pozostaje taka sama.

## Zobacz również:
- [Dokumentacja Java - klasa File](https://docs.oracle.com/javase/10/docs/api/java/io/File.html)
- [Dokumentacja Java - klasa Scanner](https://docs.oracle.com/javase/10/docs/api/java/util/Scanner.html)
- [Wprowadzenie do czytania plików w Javie](https://www.w3schools.com/java/java_files.asp)