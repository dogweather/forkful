---
title:                "Tworzenie pliku tekstu"
html_title:           "Java: Tworzenie pliku tekstu"
simple_title:         "Tworzenie pliku tekstu"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

"## Co i Dlaczego?"
Pisanie pliku tekstowego to proces, w którym programiści tworzą plik zawierający tekstowe informacje. Jest to powszechna praktyka w programowaniu, ponieważ umożliwia przechowywanie danych w trwałej formie, dzięki czemu można je łatwo odczytać i wykorzystać w przyszłości.

"## Jak to zrobić:"
Poniżej przedstawiono prosty przykład kodu Java, który demonstruje, jak utworzyć nowy plik tekstowy i zapisać w nim dane.

```Java
import java.io.FileWriter;

public class WriteTextFile{
   public static void main(String[] args) {
      try {
         // Tworzenie nowego pliku tekstowego 
         FileWriter myWriter = new FileWriter("test.txt");
         // Zapisywanie tekstu do pliku
         myWriter.write("To jest przykładowy tekst, który zostanie zapisany w pliku.");
         // Zamykanie pliku
         myWriter.close();
         System.out.println("Plik został pomyślnie zapisany!");
      } catch (Exception e) {
         System.out.println("Wystąpił błąd przy zapisywaniu do pliku.");
         e.printStackTrace();
      }
   }
}
```

Powyższy kod utworzy nowy plik tekstowy o nazwie "test.txt" i zapisze w nim podany tekst. Po uruchomieniu programu, w folderze z projektem powinien pojawić się nowy plik tekstowy zawierający wprowadzone dane.

"## Głębszy zanurzenie:"
Pisanie plików tekstowych jest popularną metodą w programowaniu od lat. Zanim powstały nowoczesne bazy danych, programiści wykorzystywali pliki tekstowe do przechowywania i przetwarzania danych. Dziś istnieją również alternatywne metody zapisywania danych, takie jak bazy danych czy format JSON. Jednak w wielu przypadkach, pisanie plików tekstowych jest wciąż wygodnym i prostym sposobem na przechowywanie danych.

"## Zobacz także:"
Jeśli chcesz dowiedzieć się więcej o pisaniu plików tekstowych w Java, polecam przeczytać ten artykuł: [Java FileWriter](https://www.geeksforgeeks.org/filewriter-java-examples/) oraz zapoznać się z dokumentacją klasy [FileWriter](https://docs.oracle.com/javase/7/docs/api/java/io/FileWriter.html).